open Base

type bril_type =
  | BrilType of string
  | BrilStructType of (string * bril_type) list
[@@deriving compare, hash, sexp]

let rec bril_type_of_yojson = function
  | `String s -> Ok (BrilType s)
  | `Assoc fields ->
      let rec parse_fields acc = function
        | [] -> Ok (BrilStructType (List.rev acc))
        | (k, v) :: rest -> (
            match bril_type_of_yojson v with
            | Ok t -> parse_fields ((k, t) :: acc) rest
            | Error e -> Error e)
      in
      parse_fields [] fields
  | _ -> Error "bril_type"

let rec bril_type_to_yojson = function
  | BrilType s -> `String s
  | BrilStructType fields ->
      `Assoc (List.map ~f:(fun (k, t) -> (k, bril_type_to_yojson t)) fields)

let rec bril_type_to_string = function
  | BrilType s -> s
  | BrilStructType fields ->
      let field_strs =
        List.map ~f:(fun (k, t) -> k ^ ": " ^ bril_type_to_string t) fields
      in
      "{" ^ String.concat ~sep:", " field_strs ^ "}"
