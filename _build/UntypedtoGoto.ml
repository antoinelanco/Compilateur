(* Transformation de la syntaxe abstraite non typée
   vers la syntaxe abstraite "goto". *)
module S = UntypedAst
module T = GotoAst

let destructure_main p =

  (* new_label: unit -> string *)
  (* Un appel [new_label()] crée une nouvelle étiquette qui peut être
     utilisée pour créer des sauts. *)
  let new_label =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "_label_main_%i" !cpt
  in

  (* destructure_block: S.block -> T.block *)
  let rec destructure_block = function
    | []     -> []
    | i :: b -> destructure_instruction i @ (destructure_block b)

  (* destructure_instruction: S.instruction -> T.block *)
  and destructure_instruction : S.instruction -> T.block = function
    | ProcCall(c) -> [ T.ProcCall(c) ]
    | Print(e)  -> [ T.Print(e)  ]
    | Set(l, e) -> [ T.Set(l,e) ]
    | If(e,b1,b2) -> let i = new_label() in
                     let j = new_label() in
                    [ T.CondGoto(e,i) ] @
                    (destructure_block b2) @
                    [ T.Goto(j);
                      T.Label(i) ] @
                    (destructure_block b1) @
                    [ T.Label(j) ]

    | While(e,b) -> let i = new_label() in
                    let j = new_label() in
                    [ T.Goto(i);
                      T.Label(j) ] @
                    (destructure_block b) @
                    [T.Label(i);
                     T.CondGoto(e,j)]

  in

  { T.locals = p.S.locals; T.code = destructure_block p.S.code }
