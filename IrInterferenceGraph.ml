open IrAst
open IrLiveness

(**
   Construction du graphe d'interférence :

   1. Pour chaque instruction définissant une variable a, hors copies,
   où les variables vivantes en sortie sont b₁, ..., bₖ, ajouter les
   arêtes (a, b₁), ..., (a, bₖ).

   2. Pour chaque instruction de copie a ← c, où les variables vivantes
   en sortie sont b₁, ..., bₖ, ajouter les arêtes (a, b₁), ..., (a, bₖ)
   pour les bᵢ distincts de c.
*)

(* Fonction auxiliaire : ajoute à un graphe l'ensemble des interférences
   dues à une instruction donnée, connaissant l'ensemble des variables
   vivantes en sortie de cette instruction. *)

let add_interferences_formals_id g v_l =
  let add_edge i1 i2 g = Printf.printf "add edge %s %s\n" i1 i2;Graph.add_edge g i1 i2 in
  let rec aux v_list acc =
    match v_list with
    | [] -> acc
    | elt::tl -> aux tl (List.fold_left (fun a e -> add_edge elt e a) acc tl)
  in aux v_l g

let add_interferences_formals g v_l =
  let add_edge id1 id2 g =
    match id1, id2 with
    | Identifier i1, Identifier i2 -> Graph.add_edge g i1 i2
    | _ -> g
  in
  let rec aux v_list acc =
    match v_list with
    | [] -> acc
    | elt::tl -> aux tl (List.fold_left (fun a e -> add_edge elt e a) acc tl)
  in aux v_l g

let add_interferences g lv_out_at_node = function
  | Binop(a, _, Identifier v1, Identifier v2) ->
    Graph.add_edge (VarSet.fold (fun e acc -> Graph.add_edge acc a e) lv_out_at_node g) v1 v2
  | Binop (a,_,_,_) | Value(a, _) ->
    VarSet.fold (fun e acc -> Graph.add_edge acc a e) lv_out_at_node g
  | ProcCall(_,v) -> add_interferences_formals g v
  | FunCall(i,_,v) ->
    let tmp = VarSet.fold
        (fun elt acc -> Graph.add_edge acc i elt)
        lv_out_at_node g
    in
    add_interferences_formals tmp v
  | _ -> g

(* Fonction principale, qui itère sur l'ensemble des points du programme. *)
let interference_graph p : Graph.t =
  (* D'abord, définir le graphe sans arêtes contenant un sommet pour chaque
     identifiant de la table des symboles. *)
  let g1 = Symb_Tbl.fold (fun v _ acc -> Graph.add_node acc v) p.locals Graph.empty in
  let g = add_interferences_formals_id g1 p.formals in
  (* Ensuite, récupérer le résultat de l'analyse de vivacité. *)
  let _, lv_out = mk_lv p in
  (* Enfin, itérer sur l'ensemble des points du programme. *)
  (* À compléter *)
  List.fold_left (fun acc (lab, instr) ->
      add_interferences acc (Hashtbl.find lv_out lab) instr) g p.code
