open AllocatedAst
open Mips


let generate_fun p =

  (* Affecte des emplacements mémoire aux variables locales. *)
  let sp_off   = p.offset in
  let symb_tbl = p.locals in
  let find_alloc id =
    try  AllocatedAst.Symb_Tbl.find id symb_tbl
    with Not_found -> failwith (Printf.sprintf "Node %s not found" id)
  in

  let rec generate_block = function
    | []       -> nop
    | (l,i)::b -> comment l @@ generate_instr i @@ generate_block b

  (* Un appel [load_value r v] génère du code qui place la valeur [v]
     dans le registre [r]. *)
  and load_value r : AllocatedAst.value -> 'a Mips.asm = function
    | Identifier(id) -> (match find_alloc id with
        | Stack o -> lw r o ~$fp
        | Reg r1 -> move r r1)


    | Literal(l) -> (match l with
        | Int i -> li r i
        | Bool b -> let i = if b then 1 else 0 in
          li r i)


  and load_value_bis r : AllocatedAst.value -> (register * 'a Mips.asm) = function
    | Identifier(id) -> (match find_alloc id with
        | Stack o -> (r,lw r o ~$fp)
        | Reg r1 -> (r1,nop))

    | Literal(l) -> (match l with
        | Int i -> (r,li r i)
        | Bool b -> let i = if b then 1 else 0 in
          (r,li r i))


  and store_stack o : AllocatedAst.value -> 'a Mips.asm = function
    | Identifier(id) -> (match find_alloc id with
        | Stack o -> lw ~$t0 o ~$fp @@ sw ~$t0 o ~$fp
        | Reg r1 -> sw r1 o ~$fp)

    | Literal(l) -> (match l with
        | Int i -> li ~$t0 i @@ sw ~$t0 o ~$fp
        | Bool b -> let i = if b then 1 else 0 in
          li ~$t0 i @@ sw ~$t0 o ~$fp)




  and store_reg r : AllocatedAst.value -> 'a Mips.asm = function
    | Identifier(id) -> (match find_alloc id with
        | Stack o -> lw r o ~$fp
        | Reg r1 -> if r1 <> r then move r r1 else nop )

    | Literal(l) -> (match l with
        | Int i -> li r i
        | Bool b -> let i = if b then 1 else 0 in
          li r i)


  and generate_instr : AllocatedAst.instruction -> 'a Mips.asm = function
    | FunCall(i,s,v) ->

      let save_args, nb = List.fold_left (fun (acc,i) e ->
          (let reg1, inst1 = load_value_bis ~$t0 e in

           acc
           @@ inst1
           @@ sw reg1 (-i * 4 - 4) ~$sp,i+1) )

          (nop,0) v
      in

      let stack_args = nb*4 in

      let max_reg = Symb_Tbl.fold
          (fun id alloc_info acc ->
             match alloc_info with
               Reg s -> let index = int_of_string (String.sub s 2 1) in
               if index > acc then index else acc
             | _ -> acc)
          p.locals 0
      in

      let save_reg reg =
        let rec aux r =
          match r with
          | 2 -> sw ("$t"^string_of_int(r)) (-4) sp
          | n -> aux (n-1) @@ sw ("$t"^string_of_int(r)) (-(n-1) * 4) sp
        in
        aux reg @@ addi sp sp (-(reg-1) * 4)
      in

      let load_reg reg =
        let rec aux r =
          match r with
          | 2 -> lw ("$t"^string_of_int(r)) (-4) sp
          | n -> aux (n-1) @@ lw ("$t"^string_of_int(r)) (-(n-1) * 4) sp
        in
        addi sp sp ((reg-1) * 4) @@ aux reg in

      let load_res =
        match find_alloc i with
        | Reg r -> move r a0
        | Stack o -> sw a0 o ~$fp
      in

      (*Etape 1*)
      save_reg max_reg
      @@ save_args
      @@ addi sp sp (-stack_args)
      @@ jal s
      (*Etape 4*)
      @@ addi sp sp stack_args
      @@ load_reg max_reg
      @@ load_res

    | ProcCall(s,v) ->

      let save_args, nb = List.fold_left (fun (acc,i) e ->
          (let reg1, inst1 = load_value_bis ~$t0 e in

           acc
           @@ inst1
           @@ sw reg1 (-i * 4 - 4) ~$sp,i+1) )

          (nop,0) v
      in

      let stack_args = nb*4 in

      let max_reg = Symb_Tbl.fold
          (fun id alloc_info acc ->
             match alloc_info with
               Reg s -> let index = int_of_string (String.sub s 2 1) in
               if index > acc then index else acc
             | _ -> acc)
          p.locals 0
      in

      let save_reg reg =
        let rec aux r =
          match r with
          | 2 -> sw ("$t"^string_of_int(r)) (-4) sp
          | n -> aux (n-1) @@ sw ("$t"^string_of_int(r)) (-(n-1) * 4) sp
        in
        aux reg @@ addi sp sp (-(reg-1) * 4)
      in

      let load_reg reg =
        let rec aux r =
          match r with
          | 2 -> lw ("$t"^string_of_int(r)) (-4) sp
          | n -> aux (n-1) @@ lw ("$t"^string_of_int(r)) (-(n-1) * 4) sp
        in
        addi sp sp ((reg-1) * 4) @@ aux reg in

      (*Etape 1*)
      save_reg max_reg
      @@ save_args
      @@ addi sp sp (-stack_args)
      @@ jal s
      (*Etape 4*)
      @@ addi sp sp stack_args
      @@ load_reg max_reg

    | Load (_,_) -> nop
    | Store (_,_) -> nop
    | New (_,_) -> nop
    | Print(v) -> load_value ~$a0 v @@ li ~$v0 11 @@ syscall
    | Goto(l) -> b l
    | Label(l) -> label l
    | Comment(str) -> comment str
    (* | CondGoto(v,l) -> load_value ~$t0 v @@ bnez ~$t0 l
       | Value(i,v) -> load_value ~$t0 v @@ (match find_alloc i with
                                          | Stack o -> sw ~$t0 o ~$fp
                                          | Reg r -> move r ~$t0)
       | Binop(i,b,v1,v2) -> load_value ~$t0 v1 @@
                          load_value ~$t1 v2 @@
                          (match b with
                            | Add -> add
                            | Sub -> sub
                            | Mult -> mul
                            | Div -> div
                            | Eq -> seq
                            | Neq -> sne
                            | Lt -> slt
                            | Le -> sle
                            | Mt -> sgt
                            | Me -> sge
                            | And -> and_
                            | Or -> or_) ~$t0 ~$t0 ~$t1 @@
                            (match find_alloc i with
                              | Stack o -> sw ~$t0 o ~$fp
                              | Reg r -> move r ~$t0)


    *)



    (*2.2.1. Méthode ad hoc *)

    | CondGoto(v,l) -> let reg ,inst = load_value_bis ~$t0 v in
      inst @@ bnez reg l

    | Value(i,v) -> (match find_alloc i with
        | Stack o -> store_stack o v
        | Reg r -> store_reg r v)

    | Binop(i,b,v1,v2) ->
      let reg1, inst1 = load_value_bis ~$t0 v1 in
      let reg2, inst2 = load_value_bis ~$t1 v2 in
      inst1 @@
      inst2 @@
      (match find_alloc i with
       | Stack o -> (match b with
           | Add -> add
           | Sub -> sub
           | Mult -> mul
           | Div -> div
           | Eq -> seq
           | Neq -> sne
           | Lt -> slt
           | Le -> sle
           | Mt -> sgt
           | Me -> sge
           | And -> and_
           | Or -> or_)~$t0 reg1 reg2 @@ sw ~$t0 o ~$fp
       | Reg r -> (match b with
           | Add -> add
           | Sub -> sub
           | Mult -> mul
           | Div -> div
           | Eq -> seq
           | Neq -> sne
           | Lt -> slt
           | Le -> sle
           | Mt -> sgt
           | Me -> sge
           | And -> and_
           | Or -> or_) r reg1 reg2 )



  in

  let load_args =
    let nb_args = List.length p.formals in
    let args, index = List.fold_left
        (fun (v,i) arg -> match find_alloc arg with
           | Reg r -> (v @@ lw r ((nb_args-i)*4 +4) ~$fp ,i+1)
           | Stack o -> (v @@ lw ~$t0 ((nb_args-i)*4 +4) ~$fp@@ sw ~$t0 o ~$fp ,i+1) )
        (nop,0) (List.rev p.formals)
    in
    args
  in

  let store_res =
    if Symb_Tbl.mem "result" p.locals
    then
      match find_alloc "result" with
      | Reg r -> move a0 r
      | Stack o -> lw a0 o ~$fp
    else nop
  in

  let start_fun = (*Etape 2*)

    sw fp (-4) sp
    @@ sw ra (-8) sp
    @@ addi sp sp (-8)
    @@ move fp sp
    @@ addi sp sp sp_off
    @@ load_args
  in

  let end_fun = (*Etape 3*)
    store_res
    @@ lw ra 0 fp
    @@ lw fp 4 fp
    @@ addi sp sp (-sp_off+8)
    @@ jr ra
  in

  start_fun @@ (generate_block p.code) @@ end_fun

let init =
  move fp sp
  @@ lw a0 0 a1
  @@ jal "atoi"
  (*@@ move a0 v0*)
  @@ sw v0 0 sp
  @@ jal "main"

let close = li v0 10 @@ syscall

let built_ins =
  label "atoi"
  @@ move t0 a0
  @@ li   t1 0
  @@ li   t2 10
  @@ label "atoi_loop"
  @@ lbu  t3 0 t0
  @@ beq  t3 zero "atoi_end"
  @@ li   t4 48
  @@ blt  t3 t4 "atoi_error"
  @@ li   t4 57
  @@ bgt  t3 t4 "atoi_error"
  @@ addi t3 t3 (-48)
  @@ mul  t1 t1 t2
  @@ add  t1 t1 t3
  @@ addi t0 t0 1
  @@ b "atoi_loop"
  @@ label "atoi_error"
  @@ li   v0 10
  @@ syscall
  @@ label "atoi_end"
  @@ move v0 t1
  @@ jr   ra


let generate_prog p =
  let asm = Symb_Tbl.fold (fun i info acc ->
      acc @@ (label i) @@ (generate_fun info) ) p nop in

  { text = init @@ close @@ asm @@ built_ins; data = nop }
