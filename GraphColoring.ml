(**
   Les couleurs sont des entiers.
   Une coloration associe les identifiants à des couleurs.
*)
module NodeMap = Map.Make(String)
type color = int
type coloring = color NodeMap.t

(** Plus grande couleur utilisée dans une coloration.
    Sera utile plus tard pour initialiser le pointeur de pile. *)
let max_color coloring =
  NodeMap.fold (fun _ c mc -> max c mc) coloring 0

(** Étant donnés un graphe partiellement coloré et un sommet [n], renvoie la
    plus petite couleur pouvant être affectée à [n]. On supposera que les
    voisins de [n] sont déjà tous colorés. *)
let pick_color g coloring n =
  (* À compléter *)
  let voisin = Graph.neighbours g n in
  let int_voisin = List.sort (-) (List.fold_left (
      fun acc i -> (NodeMap.find i coloring)::acc) [] voisin) in

  List.fold_left (fun acc i -> if i == acc then acc + 1 else acc ) 0 int_voisin

(** Renvoie une coloration pour le graphe [g]. *)
let rec colorize (g : Graph.t) : coloring =
  (* À compléter *)

  let min_som = Graph.min_degree g in
  match min_som with
  | None -> NodeMap.empty
  | Some s -> let g' = Graph.del_node g s in
    let c = colorize g' in
    let new_color = pick_color g c s in
    NodeMap.add s new_color c
