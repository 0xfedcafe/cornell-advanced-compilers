open Base
open Bril_instruction
open Instruction
open Bril_immediate

let eval_binary (o : Op.binary) (a : bril_immediate) (b : bril_immediate) :
    bril_immediate option =
  match (o, a, b) with
  | Add, BrilInt a, BrilInt b -> Some (BrilInt (a + b))
  | Sub, BrilInt a, BrilInt b -> Some (BrilInt (a - b))
  | Mul, BrilInt a, BrilInt b -> Some (BrilInt (a * b))
  | Div, BrilInt _, BrilInt 0 -> None
  | Div, BrilInt a, BrilInt b -> Some (BrilInt (a / b))
  | Eq, BrilInt a, BrilInt b -> Some (BrilBool (a = b))
  | Eq, BrilBool a, BrilBool b -> Some (BrilBool (Bool.equal a b))
  | Lt, BrilInt a, BrilInt b -> Some (BrilBool (a < b))
  | Gt, BrilInt a, BrilInt b -> Some (BrilBool (a > b))
  | Le, BrilInt a, BrilInt b -> Some (BrilBool (a <= b))
  | Ge, BrilInt a, BrilInt b -> Some (BrilBool (a >= b))
  | And, BrilBool a, BrilBool b -> Some (BrilBool (a && b))
  | Or, BrilBool a, BrilBool b -> Some (BrilBool (a || b))
  | _ -> None

let eval_unary (o : Op.unary) (a : bril_immediate) : bril_immediate option =
  match (o, a) with
  | Not, BrilBool b -> Some (BrilBool (not b))
  | Not, BrilInt _ -> None
  | Id, v -> Some v

let evaluate_instruction (instr : Instruction.t)
    (env : string -> bril_immediate option) : Instruction.t option =
  let folded dst value =
    let typ =
      match value with
      | BrilInt _ -> Bril_type.BrilType "int"
      | BrilBool _ -> Bril_type.BrilType "bool"
    in
    Const { dst; typ; value }
  in
  match instr with
  | Binary (o, { dst; src1; src2; _ }) -> (
      match (env src1, env src2) with
      | Some a, Some b -> eval_binary o a b |> Option.map ~f:(folded dst)
      | _ -> None)
  | Unary (o, { dst; src1; _ }) ->
      env src1 |> Option.bind ~f:(eval_unary o) |> Option.map ~f:(folded dst)
  | Const _ -> Some instr
  | _ -> None
