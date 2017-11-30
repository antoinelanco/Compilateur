(* Traduction de la syntaxe abstraite "goto"
   vers la représentation intermédiaire. *)
module S = GotoAst
module T = IrAst


let flatten_func p =

  (* On extrait la table des symboles de notre programme, qui sera étendue
     avec les registres virtuels créés à la volée. *)
  let symb_tbl = ref p.S.locals in

  (* Ajout à la table des symboles d'un nouveau registre virtuel *)
  let add_symb s =
    symb_tbl := T.Symb_Tbl.add s (Local: T.identifier_info) !symb_tbl;
  in

  (* new_tmp: unit -> string *)
  (* Un appel [new_tmp()] crée un nouvel identifiant de registre virtuel
     et l'ajoute à la table des symboles. *)
  let new_tmp =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      let tmp = Printf.sprintf "_tmp_%i" !cpt in
      add_symb tmp;
      tmp
  in

  (* flatten_block: S.block -> T.instruction list *)
  let rec flatten_block = function
    | []   -> []
    | i::b -> flatten_instruction i @ (flatten_block b)

  (* flatten_instruction: S.instruction -> T.instruction list *)
  and flatten_instruction = function
    | S.ProcCall(c) -> let str,args = c in
      let (es, vs) = List.fold_left (fun (es_acc, vs_acc) arg ->
          let (c,v) = flatten_expression arg in (es_acc@c,vs_acc@[v])) ([], []) args in
      es@[ T.ProcCall(str,vs) ]
    | S.Print(e) ->
      let ce, ve = flatten_expression e in
      ce @ [ T.Print(ve) ]

    | S.Set(l,e) ->
      let ce, ve = flatten_expression e in
      (match l with
       | Identifier(s) -> ce @ [ T.Value(s,ve) ]
       | ArrayAccess(e1, e2) ->
         let ce1,ve1 = flatten_expression e1 in
         let ce2,ve2 = flatten_expression e2 in
         ce @ ce1 @ ce2 @ [ T.Store( (ve1,ve2),ve ) ])

    | S.CondGoto(e,l) ->
      let ce, ve = flatten_expression e in
      ce @ [ T.CondGoto(ve,l) ]
    | S.Label(l) -> [ T.Label(l) ]
    | S.Goto(l) -> [ T.Goto(l) ]
    | S.Comment(s) -> [ T.Comment(s) ]

  (* flatten_expression: S.expression -> T.instruction list -> T.value *)
  (* Appliquée à une expression, [flatten_expression] renvoie une liste
     d'instructions calculant le résultat de cette expression, ainsi qu'une
     valeur contenant ce résultat.
     Cas représentatifs :
     - l'expression est déjà une valeur, la liste d'instructions sera vide
       et l'expression sera retournée elle-même ;
     - l'expression est composée, et la valeur sera l'identifiant du registre
       virtuel dans lequel a été placé le résultat.
  *)
  and flatten_expression : S.expression -> T.instruction list * T.value =
    function
    | FunCall(c) -> let str,args = c in
      let i = new_tmp() in
      let es, vs = List.fold_left (fun (es_acc, vs_acc) arg ->
          let c,v = flatten_expression arg in (es_acc@c,vs_acc@[v])) ([], []) args in
      es@[ T.FunCall(i,str,vs) ], T.Identifier(i)

    | Location(l) ->
      (match l with
       | Identifier(s) -> [], T.Identifier(s)
       | ArrayAccess(e1,e2) ->
         let ce1,ve1 = flatten_expression e1 in
         let ce2,ve2 = flatten_expression e2 in
         let i = new_tmp() in
         ce1 @ ce2 @ [ T.Load(i,(ve1,ve2)) ], T.Identifier(i))

    | NewArray(e) ->
      let ce,ve = flatten_expression e in
      let i = new_tmp() in
      ce @ [ T.New(i,ve) ], T.Identifier(i)
    | NewArrayAcol(es) ->
      let i = new_tmp() in
      let arraycomplet, _ =
        List.fold_left (fun (acc,nb) e -> let ce,ve = flatten_expression e in
                         (acc @ ce @ [ T.Store((T.Identifier i,T.Literal(Int(nb))),ve) ]),nb+1) ([],0) es
      in
      [ T.New(i,Literal(Int(List.length es))) ] @ arraycomplet, T.Identifier(i)
    | Literal(l) -> [], T.Literal(l)
    | Binop(b,e1,e2) -> let i = new_tmp() in
      let ce1, ve1 = flatten_expression e1 in
      let ce2, ve2 = flatten_expression e2 in
      ce1 @ ce2 @ [ T.Binop(i,b,ve1,ve2) ], T.Identifier(i)
  in

  (* label_instruction: T.instruction -> T.label * T.instruction *)
  (* Un appel [label_instruction i] crée une nouvelle étiquette pour
     identifier l'instruction [i], si celle-ci n'est pas déjà une étiquette
     de saut. *)

  let label_instruction =
    let cpt = ref 0 in
    fun i -> let lab = Printf.sprintf "_prog_%d" !cpt in
      incr cpt;
      match i with
      (* On force une correspondance entre étiquette de saut
         		  et étiquette d'analyse. *)
      | T.Label l -> l, i
      | _         -> lab, i
  in

  let flattened_code = flatten_block p.S.code in

  { T.formals = p.S.formals; T.locals = !symb_tbl; T.code = List.map label_instruction flattened_code }

let flatten_prog p =
  S.Symb_Tbl.fold (fun i info acc -> T.Symb_Tbl.add i (flatten_func info) acc ) p T.Symb_Tbl.empty
