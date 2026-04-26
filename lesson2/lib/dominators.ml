open Base
open Basic_block
open Cfg
open Lattice

module Dominators = struct
  let reverse_post_order (cfg : CFG.t) bb_id =
    let rec traverse_it (cfg : CFG.t) visited acc id =
      if Hashtbl.mem visited id then acc
      else (
        Hashtbl.set visited ~key:id ~data:();
        let node = Hashtbl.find_exn cfg.graph id in
        let acc' =
          List.fold_left node.succs ~init:acc ~f:(fun a succ_id ->
              (traverse_it [@tailcall]) cfg visited a succ_id)
        in
        id :: acc')
    in
    let visited = Hashtbl.create (module Basic_block.Id) in
    traverse_it cfg visited [] bb_id

  type dom_set = (Basic_block.id, Basic_block.Id.comparator_witness) Set.t

  module DominatorsLatticeParams = struct
    type set_t = dom_set

    let empty = Set.empty (module Basic_block.Id)
    let is_subset = Set.is_subset
    let equal = Set.equal
    let meet_sets = Set.inter
    let join_sets = Set.union
  end

  module DominatorsLattice = MakeSetLattice (DominatorsLatticeParams)

  module DominatorsAnalysis = struct
    include DominatorsLattice

    let d = Forward
    let init_state = top
    let boundary_state = bottom

    let transfer (bb : Basic_block.t) (in' : t) : t =
      match in' with Top -> Top | Set s -> Set (Set.add s bb.id)
  end

  module Solver = OrderedSolver (DominatorsAnalysis)

  type doms_state = Solver.state

  let compute_dominators (cfg : CFG.t) entry_id : doms_state =
    let state = Solver.create_state () in
    let ordered_nodes = reverse_post_order cfg entry_id in
    Solver.analyze state cfg entry_id ordered_nodes

  let compute_idoms (cfg : CFG.t) (entry_id : Basic_block.id) :
      (Basic_block.id, Basic_block.id option) Hashtbl.t =
    let idoms = Hashtbl.create (module Basic_block.Id) in
    let doms = compute_dominators cfg entry_id in

    let doms_map = doms.out' in
    Hashtbl.iteri doms_map ~f:(fun ~key:bb_id ~data:dom_set ->
        match dom_set with
        | Top -> Hashtbl.set idoms ~key:bb_id ~data:None
        | Set s ->
            let idom =
              List.fold_left (Set.elements s) ~init:None ~f:(fun acc c ->
                  if c = bb_id then acc
                  else
                    match acc with
                    | None -> Some c
                    | Some nearest_yet -> (
                        let dom_c = Hashtbl.find_exn doms_map c in
                        let dom_nearest_yet =
                          Hashtbl.find_exn doms_map nearest_yet
                        in
                        match (dom_c, dom_nearest_yet) with
                        | Top, _ -> Some c
                        | _, Top -> Some nearest_yet
                        | Set s1, Set s2 ->
                            if Set.length s1 > Set.length s2 then Some c
                            else Some nearest_yet))
            in
            Hashtbl.set idoms ~key:bb_id ~data:idom);
    idoms

  let dominates (state : doms_state) (a : Basic_block.id) (b : Basic_block.id) :
      bool =
    match Hashtbl.find state.out' b with
    | Some (Set s) -> Set.mem s a
    | Some Top -> true
    | None -> false

  let strictly_dominates (state : doms_state) (a : Basic_block.id)
      (b : Basic_block.id) : bool =
    a <> b && dominates state a b

  let dominance_frontier (cfg : CFG.t) (entry_id : Basic_block.id) :
      (Basic_block.id, dom_set) Hashtbl.t =
    let frontier = Hashtbl.create (module Basic_block.Id) in
    let doms = compute_dominators cfg entry_id in

    Hashtbl.iter_keys cfg.graph ~f:(fun bb_id ->
        Hashtbl.set frontier ~key:bb_id
          ~data:(Set.empty (module Basic_block.Id)));

    Hashtbl.iteri cfg.graph ~f:(fun ~key:a ~data:node ->
        List.iter node.succs ~f:(fun b ->
            let a_doms =
              match Hashtbl.find_exn doms.out' a with
              | Top -> []
              | Set s -> Set.elements s
            in
            List.iter a_doms ~f:(fun d ->
                if not (strictly_dominates doms d b) then
                  let current_frontier = Hashtbl.find_exn frontier d in
                  Hashtbl.set frontier ~key:d ~data:(Set.add current_frontier b))));

    frontier
end
