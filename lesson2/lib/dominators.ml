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
    Solver.analyze state cfg ordered_nodes

  let dominates (state : doms_state) (a : Basic_block.id) (b : Basic_block.id) :
      bool =
    match Hashtbl.find state.out' b with
    | Some (Set s) -> Set.mem s a
    | Some Top -> true
    | None -> false
end
