(* Transformation de la syntaxe abstraite typée
   en syntaxe abstraite non typée. *)

module S = SourceAst  (* Source de la transformation *)
module T = UntypedAst (* Cible de la transformation  *)

(* erase_identifier_info: S.identifier_info -> T.identifier_info *)
let erase_identifier_info i = i.S.kind

(* let erase_main p =
   let locals =
    S.Symb_Tbl.fold
      (fun id info tbl ->
   	T.Symb_Tbl.add id (erase_identifier_info info) tbl)
      p.S.locals
      T.Symb_Tbl.empty
   in
   { T.locals = locals; T.code = p.S.code } *)

let erase_prog p =
  S.Symb_Tbl.fold (
    fun i info acc -> let locals = S.Symb_Tbl.fold (
                        fun id inf tbl ->
                        T.Symb_Tbl.add id (erase_identifier_info inf) tbl)
                        info.S.locals T.Symb_Tbl.empty in

                      let formals = List.fold_left
                      (fun acc (_,id) -> acc@[id])
                      [] info.S.formals in

      T.Symb_Tbl.add i {T.formals = formals; T.locals = locals; T.code = info.S.code} acc) p T.Symb_Tbl.empty
