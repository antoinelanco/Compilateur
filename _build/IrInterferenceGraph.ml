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
let add_interferences g lv_out_at_node = function
  | Binop(a, _, Identifier v1, Identifier v2) ->
      Graph.add_edge (VarSet.fold (fun e acc -> Graph.add_edge acc a e) lv_out_at_node g) v1 v2
  | Binop (a,_,_,_) | Value(a, _) ->
      VarSet.fold (fun e acc -> Graph.add_edge acc a e) lv_out_at_node g
  | _ -> g

(* Fonction principale, qui itère sur l'ensemble des points du programme. *)
let interference_graph p : Graph.t =
  (* D'abord, définir le graphe sans arêtes contenant un sommet pour chaque
     identifiant de la table des symboles. *)
  let g = Symb_Tbl.fold (fun v _ acc -> Graph.add_node acc v) p.locals Graph.empty in
  (* Ensuite, récupérer le résultat de l'analyse de vivacité. *)
  let _, lv_out = mk_lv p in
  (* Enfin, itérer sur l'ensemble des points du programme. *)
  (* À compléter *)
  List.fold_left (fun acc (lab, instr) ->
     add_interferences acc (Hashtbl.find lv_out lab) instr) g p.code