open IrAst
open IrLiveness

(* Élimination des instructions mortes.
   Renvoie un couple [(f, p)] où [p] est le programme simplifié, et
   où le booléen [b] vaut [true] si au moins une instruction a été
   éliminée.
     [dce_step: IrAst.main -> bool * IrAst.main]
*)
let dce_step p =

  (* Calcul des informations de vivacité *)
  let _, lv_out = mk_lv p in

  (* À partir d'une instruction et de son étiquette, répond [true]
     si l'instruction est vivante, et [false] sinon.
       [live_instr: IrAst.label * IrAst.instruction -> bool]

     Une instruction morte est une instruction affectant une valeur à
     un registre virtuel qui n'est pas vivant en sortie de cette instruction.
     Toutes les autres sont vivantes.
  *)

  let var_updated = function
    | Value(id,_) | Binop(id,_,_,_) | FunCall(id,_,_) | Load(id,_) | New(id,_) ->
      if id <> "result" then VarSet.singleton id else VarSet.empty
    | _ -> VarSet.empty
  in

  let live_instr = function
    | (lab, instr) -> let tmp_out = Hashtbl.find lv_out lab in
      let update = var_updated instr in
      VarSet.subset update tmp_out
  in

  (* Filtre la liste pour ne garder que les instructions vivantes *)
  let filtered_code = List.filter live_instr p.code in
  (* Renvoie le booléen et le code simplifié *)
  List.length p.code <> List.length filtered_code, { p with code=filtered_code }


(* Élimination itérée *)
let dce p =
  let rec aux p =
    let (b,new_p) = dce_step p in
    if b then aux new_p else new_p
  in
  Symb_Tbl.fold (fun i info acc -> Symb_Tbl.add i (aux info) acc) p Symb_Tbl.empty
