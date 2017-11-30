module S = TypedAst
module T = UntypedAst
open Printf



let erase_identifier_info i = i.SourceAst.kind


let rec untyper_block b = List.fold_left
    (fun acc i -> acc @ [(untyper_instruction i)] ) [] b


and untyper_instruction = function
  | S.ProcCall(c) ->
    let c = c.elt in
    let str, el = c in
    let cc = List.fold_left(fun acc e -> acc @ [untyper_expression e] ) [] el in
    T.ProcCall(str,cc)

  | S.Set(l,e) ->
    let ll = untyper_location l in
    let ee = untyper_expression e in
    T.Set(ll,ee)

  | S.While(e,b) ->
    let ee = untyper_expression e in
    let bb = untyper_block b in
    T.While(ee,bb)

  | S.If(e,b1,b2) ->
    let ee = untyper_expression e in
    let bb1 = untyper_block b1 in
    let bb2 = untyper_block b2 in
    T.If(ee,bb1,bb2)

  | S.Print(e) ->
    let ee = untyper_expression e in
    T.Print(ee)


and untyper_expression e = match e.elt with
  | S.NewArray(e,t) ->
    let ee = untyper_expression e in
    T.NewArray(ee)

  | S.NewArrayAcol(el) ->
    let ell = List.fold_left(fun acc i -> acc @ [(untyper_expression i)]) [] el in
    T.NewArrayAcol(ell)

  | S.Literal(lit) ->
    let ll = untyper_literal lit in
    T.Literal(ll)

  | S.Location(loc) ->
    let ll = untyper_location loc in
    T.Location(ll)

  | S.Binop(op, e1, e2) ->
    let ee1 = untyper_expression e1 in
    let ee2 = untyper_expression e2 in
    T.Binop(op,ee1,ee2)

  | S.FunCall(c) ->
    let c = c.elt in
    let str, el = c in
    let cc = List.fold_left(fun acc e -> acc @ [untyper_expression e] ) [] el in
    T.FunCall(str,cc)


and untyper_literal = function
  | Int i  -> T.Int i
  | Bool b -> T.Bool b

and untyper_location l = match l.elt with
  | S.Identifier(id) -> T.Identifier(id)
  | S.ArrayAccess(e1,e2) ->
    let ee1 = untyper_expression e1 in
    let ee2 = untyper_expression e2 in
    T.ArrayAccess(ee1,ee2)



let untyper  p = S.Symb_Tbl.fold ( fun i info acc ->

    let locals = S.Symb_Tbl.fold (
        fun id inf tbl -> T.Symb_Tbl.add id (erase_identifier_info inf) tbl )
        info.S.locals T.Symb_Tbl.empty
    in

    let formals = List.fold_left (
        fun acc (_,id) -> acc@[id])
        [] info.S.formals
    in

    T.Symb_Tbl.add i
      {T.formals = formals; T.locals = locals; T.code = (untyper_block info.S.code)}
      acc) p T.Symb_Tbl.empty
