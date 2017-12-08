module S = TypedAst
module T = UntypedAst
open Printf

exception Failure of string

let rec find_struct x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | (h,_) :: t -> if x = h then 0 else 1 + find_struct x t


let erase_identifier_info i = i.SourceAst.kind
let rec untyper_block structs b = List.fold_left
    (fun acc i -> acc @ [(untyper_instruction structs i)] ) [] b

and untyper_instruction structs = function
  | S.Throw -> T.Throw
  | S.Try(b1,b2) ->
    let bb1 = untyper_block structs b1 in
    let bb2 = untyper_block structs b2 in
    T.Try(bb1,bb2)
  | S.ProcCall(c) ->
    let c = c.elt in
    let str, el = c in
    let cc = List.fold_left(fun acc e -> acc @ [untyper_expression structs e] ) [] el in
    T.ProcCall(str,cc)
  | S.Set(l,e) ->
    let ll = untyper_location structs l in
    let ee = untyper_expression structs e in
    T.Set(ll,ee)
  | S.While(e,b) ->
    let ee = untyper_expression structs e in
    let bb = untyper_block structs b in
    T.While(ee,bb)
  | S.If(e,b1,b2) ->
    let ee = untyper_expression structs e in
    let bb1 = untyper_block structs b1 in
    let bb2 = untyper_block structs b2 in
    T.If(ee,bb1,bb2)
  | S.Print(e) ->
    let ee = untyper_expression structs e in
    T.Print(ee)
and untyper_expression structs e =
  match e.elt with
  | S.NewRecord(id) ->
    let a = S.Symb_Tbl.find id structs in
    T.NewArray(Literal(Int(List.length a)))
  | S.NewArray(e,t) ->
    let ee = untyper_expression structs e in
    T.NewArray(ee)
  | S.NewArrayAcol(el) ->
    let ell = List.fold_left(fun acc i -> acc @ [(untyper_expression structs i)]) [] el in
    T.NewArrayAcol(ell)
  | S.Literal(lit) ->
    let ll = untyper_literal lit in
    T.Literal(ll)
  | S.Location(loc) ->
    let ll = untyper_location structs loc in
    T.Location(ll)
  | S.Binop(op, e1, e2) ->
    (match e1.elt,e2.elt with
     | S.Literal(SourceAst.Int(l1)),S.Literal(SourceAst.Int(l2)) ->
       T.Literal(T.Int(l1+l2))
     | _ ->
       let ee1 = untyper_expression structs e1 in
       let ee2 = untyper_expression structs e2 in
       T.Binop(op,ee1,ee2))
  | S.FunCall(c) ->
    let c = c.elt in
    let str, el = c in
    let cc = List.fold_left(fun acc e -> acc @ [untyper_expression structs e] ) [] el in
    T.FunCall(str,cc)
and untyper_literal = function
  | Int i  -> T.Int i
  | Bool b -> T.Bool b
and untyper_location structs l = match l.elt with
  | S.FieldAccess(e,id) ->
    let ee = untyper_expression structs e in
    (match e.annot with
     | TypStruct n ->
       let tabb = S.Symb_Tbl.find n structs in
       let index = find_struct id tabb in
       T.ArrayAccess(ee,Literal(Int(index)))
     | _ -> failwith "TypStruct n")

  | S.Identifier(id) -> T.Identifier(id)
  | S.ArrayAccess(e1,e2) ->
    let ee1 = untyper_expression structs e1 in
    let ee2 = untyper_expression structs e2 in
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
      {T.formals = formals; T.locals = locals; T.code = (untyper_block p.S.structs info.S.code)}
      acc) p.functions T.Symb_Tbl.empty
