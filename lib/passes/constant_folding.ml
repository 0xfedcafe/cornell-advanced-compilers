open Base
open Bril_instruction
open Instruction
open Bril_immediate

let eval_arithm (instr : bril_arithm_instr)
    (env : string -> bril_immediate option) : bril_immediate option =
  match instr with
  | Add { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilInt (a + b))
      | _ -> None)
  | Sub { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilInt (a - b))
      | _ -> None)
  | Mul { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilInt (a * b))
      | _ -> None)
  | Div { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) ->
          if b = 0 then None else Some (BrilInt (a / b))
      | _ -> None)

let eval_comp (instr : bril_comp_instr) (env : string -> bril_immediate option)
    : bril_immediate option =
  match instr with
  | Eq { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilBool (a = b))
      | Some (BrilBool a), Some (BrilBool b) -> Some (BrilBool (Bool.equal a b))
      | _ -> None)
  | Lt { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilBool (a < b))
      | _ -> None)
  | Gt { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilBool (a > b))
      | _ -> None)
  | Le { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilBool (a <= b))
      | _ -> None)
  | Ge { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilInt a), Some (BrilInt b) -> Some (BrilBool (a >= b))
      | _ -> None)

let eval_logic (instr : bril_logic_instr)
    (env : string -> bril_immediate option) : bril_immediate option =
  match instr with
  | Not { src1; _ } -> (
      match env src1 with
      | Some (BrilBool a) -> Some (BrilBool (not a))
      | _ -> None)
  | And { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilBool a), Some (BrilBool b) -> Some (BrilBool (a && b))
      | _ -> None)
  | Or { src1; src2; _ } -> (
      match (env src1, env src2) with
      | Some (BrilBool a), Some (BrilBool b) -> Some (BrilBool (a || b))
      | _ -> None)

let evaluate_instruction (instr : Instruction.t)
    (env : string -> bril_immediate option) : Instruction.t option =
  let make_const dst value = Const (Const { dst; value }) in
  match instr with
  | Arithm a -> (
      match eval_arithm a env with
      | Some v ->
          let dst =
            match a with
            | Add { dst; _ } | Sub { dst; _ } | Mul { dst; _ } | Div { dst; _ }
              ->
                dst
          in
          Some (make_const dst v)
      | None -> None)
  | Comp c -> (
      match eval_comp c env with
      | Some v ->
          let dst =
            match c with
            | Eq { dst; _ }
            | Lt { dst; _ }
            | Gt { dst; _ }
            | Le { dst; _ }
            | Ge { dst; _ } ->
                dst
          in
          Some (make_const dst v)
      | None -> None)
  | Logic l -> (
      match eval_logic l env with
      | Some v ->
          let dst =
            match l with
            | Not { dst; _ } -> dst
            | And { dst; _ } | Or { dst; _ } -> dst
          in
          Some (make_const dst v)
      | None -> None)
  | Const _ -> Some instr
  | Misc (Identity { dst; src }) -> (
      match env src with Some v -> Some (make_const dst v) | None -> None)
  | _ -> None
