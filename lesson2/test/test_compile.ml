open Base

type basic_block = {
  label : string option;
  instructions : int list;
}
[@@deriving compare, hash, sexp]

module CFG = struct
  type edges = {
    preds : basic_block list;
    succs : basic_block list;
  }

  type t = {
    label_to_bb : (string, basic_block) Hashtbl.t;
    cfg : (basic_block, edges) Hashtbl.t;
  }

  let create () =
    {
      label_to_bb =
        Hashtbl.create
          (module struct
            type t = string

            let compare = String.compare
            let hash = Hashtbl.hash
            let sexp_of_t _ = Sexp.Atom "string"
          end);
      cfg =
        Hashtbl.create
          (module struct
            type t = basic_block

            let compare = Poly.compare
            let hash = Hashtbl.hash
            let sexp_of_t _ = Sexp.Atom "basic_block"
          end);
    }

  let add_edge t ~src ~dst =
    Hashtbl.update t.cfg src ~f:(function
      | None -> { preds = []; succs = [ dst ] }
      | Some e -> { e with succs = dst :: e.succs });
    Hashtbl.update t.cfg dst ~f:(function
      | None -> { preds = [ src ]; succs = [] }
      | Some e -> { e with preds = src :: e.preds })
end
