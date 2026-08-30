open Cfg
open Base

module type Lattice = sig
  type t

  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val leq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val top : t
  val bottom : t
end

type direction = Forward | Backward

module type Analysis = sig
  include Lattice

  val d : direction
  val init_state : t
  val boundary_state : t
  val transfer : Basic_block.t -> t -> t
end

module type SolverState = sig
  type t

  type state = {
    in' : (Basic_block.id, t) Hashtbl.t;
    out' : (Basic_block.id, t) Hashtbl.t;
  }

  val create_state : unit -> state
  val update_in : state -> Basic_block.id -> t -> unit
  val update_out : state -> Basic_block.id -> t -> unit
  val preds : CFG.t -> Basic_block.id -> Basic_block.id list
  val succs : CFG.t -> Basic_block.id -> Basic_block.id list
end

module BaseSolver (A : Analysis) = struct
  type t = A.t

  type state = {
    in' : (Basic_block.id, t) Hashtbl.t;
    out' : (Basic_block.id, t) Hashtbl.t;
  }

  let create_state () =
    {
      in' = Hashtbl.create (module Basic_block.Id);
      out' = Hashtbl.create (module Basic_block.Id);
    }

  let update_in state bb_id new_in =
    Hashtbl.set state.in' ~key:bb_id ~data:new_in

  let update_out state bb_id new_out =
    Hashtbl.set state.out' ~key:bb_id ~data:new_out

  let preds (cfg : CFG.t) bb_id =
    match A.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).preds
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).succs

  let succs (cfg : CFG.t) bb_id =
    match A.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).succs
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).preds

  let init_nodes s nodes entry_id =
    List.iter nodes ~f:(fun n ->
        let is_entry = n = entry_id in
        if not (Hashtbl.mem s.in' n) then
          Hashtbl.set s.in' ~key:n
            ~data:(if is_entry then A.boundary_state else A.init_state);
        if not (Hashtbl.mem s.out' n) then
          Hashtbl.set s.out' ~key:n
            ~data:(if is_entry then A.boundary_state else A.init_state))

  let process_node s cfg x entry_id =
    let in' =
      if x = entry_id then A.boundary_state
      else
        let p = preds cfg x |> List.filter ~f:(Hashtbl.mem s.out') in
        if List.is_empty p then A.boundary_state
        else
          List.fold_left p ~init:A.top ~f:(fun acc pred ->
              let p_out = Hashtbl.find_exn s.out' pred in
              A.meet p_out acc)
    in
    let bb = Hashtbl.find_exn cfg.nodes x in
    let out' = A.transfer bb in' in
    let out = Hashtbl.find_exn s.out' x in
    update_in s x in';
    update_out s x out';
    not (A.( = ) out out')
end

module DataflowSolver (A : Analysis) = struct
  include BaseSolver (A)

  let analyze (s : state) (cfg : CFG.t) (entry_id : Basic_block.id) : state =
    let ns = cfg.nodes in
    let workgroup = Hashtbl.keys ns in
    init_nodes s workgroup entry_id;

    let rec analyze_iter workgroup =
      match workgroup with
      | [] -> ()
      | x :: xs ->
          let changed = process_node s cfg x entry_id in
          if changed then
            let succs' = succs cfg x in
            analyze_iter (xs @ succs')
          else analyze_iter xs
    in
    analyze_iter workgroup;
    s
end

module OrderedSolver (A : Analysis) = struct
  include BaseSolver (A)

  let analyze (s : state) (cfg : CFG.t) (entry_id : Basic_block.id)
      (ordered_nodes : Basic_block.id list) : state =
    init_nodes s ordered_nodes entry_id;

    let rec analyze_iter () =
      let changed =
        List.fold_left ordered_nodes ~init:false ~f:(fun changed_acc x ->
            let node_changed = process_node s cfg x entry_id in
            changed_acc || node_changed)
      in
      if changed then analyze_iter ()
    in
    analyze_iter ();
    s
end

type 'a set_lattice_t = Top | Set of 'a

module type SetLatticeParams = sig
  type set_t

  val empty : set_t
  val is_subset : set_t -> of_:set_t -> bool
  val equal : set_t -> set_t -> bool
  val meet_sets : set_t -> set_t -> set_t
  val join_sets : set_t -> set_t -> set_t
end

module MakeSetLattice (P : SetLatticeParams) :
  Lattice with type t = P.set_t set_lattice_t = struct
  type t = P.set_t set_lattice_t

  let ( <= ) s1 s2 =
    match (s1, s2) with
    | _, Top -> true
    | Top, _ -> false
    | Set s1', Set s2' -> P.is_subset s1' ~of_:s2'

  let leq = ( <= )

  let ( = ) s1 s2 =
    match (s1, s2) with
    | Top, Top -> true
    | Set s1', Set s2' -> P.equal s1' s2'
    | _ -> false

  let join s1 s2 =
    match (s1, s2) with
    | Top, _ | _, Top -> Top
    | Set s1', Set s2' -> Set (P.join_sets s1' s2')

  let meet s1 s2 =
    match (s1, s2) with
    | Top, x | x, Top -> x
    | Set s1', Set s2' -> Set (P.meet_sets s1' s2')

  let top = Top
  let bottom = Set P.empty
end
