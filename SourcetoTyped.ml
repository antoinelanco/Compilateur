module S = SourceAst
module T = TypedAst
open Printf

exception Type_error of string * string * string

let comparetype (pos:Lexing.position) t1 t2 =
  if t1 <> t2
  then raise (Type_error("line : "^string_of_int(pos.pos_lnum),
                         (S.print_typ t1), (S.print_typ t2)))

let typecheck_func p tab =

  let symb_tbl = p.S.locals in

  let rec typecheck_block b = List.fold_left
      (fun acc (position, i) -> acc @ (typecheck_instruction position i) )
      [] b


  and typecheck_instruction pos = function
    | S.ProcCall(c) -> let str, el = c in

      let cc = List.fold_left (fun acc e ->
          acc @ [type_expression pos e] ) [] el in

      let new_str = List.fold_left(
          fun acc e -> sprintf "%s_%s" acc (SourceAst.print_typ e.T.annot))
          str cc
      in

      let _ = S.Symb_Tbl.find new_str tab.S.functions in

      [T.ProcCall({annot = None; elt = (new_str,cc) })]

    | S.Set(l, e) ->
      let ll = type_location pos l in
      let ee = type_expression pos e in
      comparetype pos ll.T.annot ee.T.annot;
      [Set(ll,ee)]

    | S.While(e, b) ->
      let ee = type_expression pos e in
      let bl = typecheck_block b in
      comparetype pos TypBoolean ee.T.annot;
      [T.While(ee, bl)]

    | S.If(e, b1, b2) ->
      let ee = type_expression pos e in
      let bb1 = typecheck_block b1 in
      let bb2 = typecheck_block b2 in
      comparetype pos TypBoolean ee.T.annot;
      [If(ee, bb1, bb2)]

    | S.Print(e) ->
      let ee = type_expression pos e in
      comparetype pos TypInteger ee.T.annot;
      [Print(ee)]


  and type_expression pos = function
    | S.NewRecord(id) ->
      let _ = S.Symb_Tbl.find id tab.structs in
      {annot = TypStruct(id); elt = T.NewRecord(id)}

    | S.NewArray(e,t) -> let ee = type_expression pos e in
      comparetype pos TypInteger ee.T.annot;
      {annot = t ;elt = T.NewArray(ee,t)}

    | S.NewArrayAcol(el) -> (match el with
        | e::_ -> let ee = type_expression pos e in
          (match ee.annot with
           | S.TypInteger -> let explist = List.fold_left(
               fun acc i ->
                 let ii = type_expression pos i in
                 comparetype pos S.TypInteger ii.annot; acc @ [ii] ) [] el in
             {annot=TypArray TypInteger; elt=T.NewArrayAcol(explist)}
           | S.TypBoolean -> let explist = List.fold_left(
               fun acc i ->
                 let ii = type_expression pos i in
                 comparetype pos S.TypBoolean ii.annot; acc @ [ii] ) [] el in
             {annot=TypArray TypBoolean; elt=T.NewArrayAcol(explist)}
           | _ -> failwith "integer ou bool dans {} ")
        | [] -> failwith "{} vide ")

    | S.Literal(lit)  -> {annot=(type_literal lit); elt=T.Literal(lit)}

    | S.Location(loc) -> let ll = type_location pos loc in
      {annot=ll.T.annot; elt=T.Location(ll)}

    | S.Binop(op, e1, e2) ->
      let ty_op, ty_r = type_binop op in
      let ee1 = type_expression pos e1 in
      let ee2 = type_expression pos e2 in
      comparetype pos ty_op ee1.T.annot;
      comparetype pos ty_op ee2.T.annot;
      {annot = ty_r; elt = T.Binop(op,ee1,ee2)}


    | S.FunCall(c) -> let str, el = c in

      let cc = List.fold_left (fun acc e ->
          acc @ [type_expression pos e] ) [] el in

      let new_str = List.fold_left(
          fun acc e -> sprintf "%s_%s" acc (SourceAst.print_typ e.T.annot))
          str cc
      in

      let infos = S.Symb_Tbl.find new_str tab.functions in

      match infos.S.return with
      | Some t -> {annot=t; elt=T.FunCall({annot = Some t; elt = (new_str,cc) })}
      | None -> failwith "il ny a pas de type de retour (bug FunCall)"

  and type_literal = function
    | Int i  -> TypInteger
    | Bool b -> TypBoolean

  and type_location pos = function
    | S.FieldAccess(e,id) ->
      let ee = type_expression pos e in
      (match ee.annot with
       | TypStruct n ->
         let fiel = S.Symb_Tbl.find n tab.structs in
         let ty = List.assoc id fiel in
         { annot = ty; elt = T.FieldAccess(ee,id)}
       | _ -> failwith "TypStruct n exepcted")

    | S.Identifier id ->
      {annot=(S.Symb_Tbl.find id symb_tbl).typ; elt=T.Identifier(id)}
    | S.ArrayAccess(e1,e2) ->
      let ee1 = type_expression pos e1 in
      let ee2 = type_expression pos e2 in
      comparetype pos TypInteger ee2.T.annot;
      (match ee1.T.elt with
       | T.Location i ->
         (match i.T.annot with
          | TypArray n -> {annot=n; elt=T.ArrayAccess(ee1,ee2)}
          | _ -> failwith "TypArray n expected")
       | _ -> failwith "loc[int] only (ArrayAccess)")



  and type_binop = function
    | Add | Sub | Mult | Div         -> TypInteger, TypInteger
    | Eq  | Neq | Lt  | Le | Mt | Me -> TypInteger, TypBoolean
    | And | Or                       -> TypBoolean, TypBoolean

  in
  { T.return=p.S.return;
    T.formals=p.S.formals;
    T.locals=p.S.locals;
    T.code=(typecheck_block p.S.code) }

let typer p =
  {T.functions = S.Symb_Tbl.fold (
      fun i info acc -> T.Symb_Tbl.add i (typecheck_func info p) acc)
      p.functions T.Symb_Tbl.empty; T.structs = p.structs}
