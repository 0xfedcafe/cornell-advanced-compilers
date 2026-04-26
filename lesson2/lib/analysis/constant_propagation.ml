open Base
open Bril_instruction
open Basic_block
open Cfg
open Lattice

type constant_propagation_set =
  (Instruction.t, Instruction.comparator_witness) Set.t

type constant_propagation_lattice_t = constant_propagation_set set_lattice_t

module ConstantPropagationLattice = MakeSetLattice (struct
  type set_t = constant_propagation_set

  let empty = Set.empty (module Instruction)
  let is_subset = Set.is_subset
  let equal = Set.equal

  let meet_sets s1' s2' =
    let union_set = Set.union s1' s2' in
    let dest_counts = Hashtbl.create (module String) in
    Set.iter union_set ~f:(fun instr ->
        match Instruction.get_dest instr with
        | Some dst ->
            let count =
              Hashtbl.find dest_counts dst |> Option.value ~default:0
            in
            Hashtbl.set dest_counts ~key:dst ~data:(count + 1)
        | None -> ());
    Set.filter union_set ~f:(fun instr ->
        match Instruction.get_dest instr with
        | Some dst -> Int.equal (Hashtbl.find_exn dest_counts dst) 1
        | None -> true)

  let join_sets = Set.union
end)

module ConstantPropagationAnalysis :
  Analysis with type t = ConstantPropagationLattice.t = struct
  include ConstantPropagationLattice

  let d = Forward
  let init_state = bottom
  let boundary_state = bottom

  let transfer (bb : Basic_block.t) (in' : ConstantPropagationLattice.t) :
      ConstantPropagationLattice.t =
    match in' with
    | Top -> Top
    | Set acc ->
        let gen_kill =
          List.fold_left bb.instructions ~init:acc ~f:(fun acc instr ->
              match Instruction.get_dest instr with
              | Some dst -> (
                  let filter =
                    Set.filter acc ~f:(fun i ->
                        let got_dst = Instruction.get_dest i in
                        match got_dst with
                        | Some d -> String.(d <> dst)
                        | None -> true)
                  in
                  match instr with
                  | Instruction.Const _ -> Set.add filter instr
                  | _ -> filter)
              | None -> acc)
        in
        Set gen_kill
end

module ConstantPropagationSolver = DataflowSolver (ConstantPropagationAnalysis)

let constant_propagation_analysis (cfg : CFG.t) (entry_id : Basic_block.id) :
    ConstantPropagationSolver.state =
  let module Solver = DataflowSolver (ConstantPropagationAnalysis) in
  let state = Solver.create_state () in
  ConstantPropagationSolver.analyze state cfg entry_id
