open Cfg

module Ssa : sig
  val convert_to_ssa :
    Bril.bril_ir_function -> CFG.t -> Basic_block.id -> CFG.t

  val convert_from_ssa : CFG.t -> CFG.t
end
