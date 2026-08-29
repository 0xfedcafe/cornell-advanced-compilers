open Base
open Bril_instruction
open Bril_immediate

val eval_binary :
  Op.binary -> bril_immediate -> bril_immediate -> bril_immediate option

val eval_unary : Op.unary -> bril_immediate -> bril_immediate option

val evaluate_instruction :
  Instruction.t -> (string -> bril_immediate option) -> Instruction.t option
