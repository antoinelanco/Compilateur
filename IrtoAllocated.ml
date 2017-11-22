module S = IrAst
module T = AllocatedAst
open GraphColoring
open IrInterferenceGraph
(* Allocation *)
let allocate_main reg_flag prog =
  let current_offset_stack = ref 0 in

  let tbl p =
    current_offset_stack := 0;
    if reg_flag
    (*Version 1*)
    (* then
       let current_offset_reg = ref 1 in
       S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
              match info with
                | FormalX -> T.Stack 0
                | Local -> if !current_offset_reg >= 9
                            then (current_offset_stack := (!current_offset_stack - 4); T.Stack (!current_offset_stack))
                            else (current_offset_reg := (!current_offset_reg + 1); T.Reg ("$r" ^ string_of_int(!current_offset_reg)))
                  ) p.S.locals *)

    (*Version 1'*)
    then

      let g = interference_graph p in
      (* Printf.printf "%s\n" (Graph.dump g); *)
      let g_color = colorize g in
      (* GraphColoring.NodeMap.iter
         (fun key elt -> Printf.printf "%s %d\n" key elt)
         g_color; *)

      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
          let i = NodeMap.find id g_color in
          match info with
          | Return -> (current_offset_stack := (!current_offset_stack - 4); T.Stack (!current_offset_stack))
          | _ -> if i > 7
            then (Printf.printf "var: %s <- " id ;
                  current_offset_stack := (!current_offset_stack - 4);
                  Printf.printf "Stack(%d) \n" !current_offset_stack ;
                  T.Stack (!current_offset_stack))

            else (Printf.printf "var: %s <- Reg(%s)\n" id ("$t" ^ string_of_int(i+2)) ;
                  T.Reg ("$t" ^ string_of_int(i+2)))
        ) p.S.locals

    else
      (* Tout sur la pile *)
      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
          match info with
          | _ -> Printf.printf "var: %s <- " id ;
            current_offset_stack := (!current_offset_stack - 4);
            Printf.printf "Stack(%d) \n" !current_offset_stack ;
            T.Stack (!current_offset_stack)
        ) p.S.locals
  in

  S.Symb_Tbl.fold (fun i info acc -> Printf.printf "Function : %s\n" i;
                    T.Symb_Tbl.add i
                      {T.formals = info.S.formals;
                       T.locals = tbl info;
                       T.offset= !current_offset_stack;
                       T.code = info.S.code}
                      acc)
    prog T.Symb_Tbl.empty
