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
        | Stack on -> lw ~$t0 on ~$fp @@ sw ~$t0 o ~$fp
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
    | Throw -> lw ~$t0 4 s0 @@ jr ~$t0
    | RmHandler -> lw ~$t0 0 ~$s0 @@ move ~$s0 ~$t0
    | NewHandler(l) ->
      li ~$a0 8
      @@ li ~$v0 9
      @@ syscall
      @@ la ~$t0 l
      @@ sw ~$t0 4 ~$v0
      @@ lw ~$t0 0 ~$s0
      @@ sw ~$t0 0 ~$v0
      @@ move ~$s0 ~$v0

    | FunCall(i,s,v) ->
      let stack_args = ref 0 in

      let save_args, nb = List.fold_left (fun (acc,i) e ->
          (let reg1, inst1 = load_value_bis ~$t0 e in

           acc
           @@ inst1
           @@ (if i > 3 then ( stack_args := (!stack_args+4);
                               sw reg1 ( -(i-4) * 4 - 4 ) ~$sp)
               else move ("$a"^string_of_int(i)) reg1 ),i+1 ) )

          (nop,0) v
      in

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
        | Reg r -> move r v0
        | Stack o -> sw v0 o ~$fp
      in

      (*Etape 1*)
      save_reg max_reg
      @@ save_args
      @@ addi sp sp (-(!stack_args))
      @@ jal s
      (*Etape 4*)
      @@ addi sp sp !stack_args
      @@ load_reg max_reg
      @@ load_res

    | ProcCall(s,v) ->

      let stack_args = ref 0 in

      let save_args, nb = List.fold_left (fun (acc,i) e ->
          (let reg1, inst1 = load_value_bis ~$t0 e in

           acc
           @@ inst1
           @@ (if i > 3 then (stack_args := (!stack_args+4);
                              sw reg1 ( -(i-4) * 4 - 4 ) ~$sp)
               else move ("$a"^string_of_int(i)) reg1 ),i+1 ) )

          (nop,0) v
      in


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
      @@ addi sp sp (-(!stack_args))
      @@ jal s
      (*Etape 4*)
      @@ addi sp sp !stack_args
      @@ load_reg max_reg

    | Load (id,(v1,v2)) ->

      let ce1,ve1 = load_value_bis ~$t0 v2 in


      ve1
      @@ li ~$t1 4
      @@ mul ~$t1 ce1 ~$t1


      @@ (match v1 with
          | Identifier(idd) -> (match find_alloc idd with
              | Stack o -> lw ~$t0 o ~$fp @@ add ~$t0 ~$t0 ~$t1
              | Reg r -> add ~$t0 r ~$t1 )
          | _ -> failwith "Lit[n] pas possible")


      @@ (match find_alloc id with
          | Stack oo -> lw ~$t1 4 ~$t0 @@ sw ~$t1 oo ~$fp
          | Reg rr -> lw rr 4 ~$t0 )

    | Store ((v1,v2),v) ->

      let ce1,ve1 = load_value_bis ~$t1 v in
      let ce2,ve2 = load_value_bis ~$t0 v2 in

      ve2
      @@ li ~$t1 4
      @@ mul ~$t1 ce2 ~$t1
      @@ (match v1 with
          | Identifier(id) -> (match find_alloc id with
              | Stack o -> lw ~$t0 o ~$fp @@ add ~$t0 ~$t0 ~$t1 @@ ve1 @@ sw ce1 4 ~$t0
              | Reg r -> add ~$t0 r ~$t1 @@ ve1 @@ sw ce1 4 ~$t0)
          | _ -> failwith "Lit[n] pas possible")


    | New (id,v) ->

      let ce1,ve1 = load_value_bis ~$t1 v in
      ve1
      @@ li ~$t0 4
      @@ mul ~$t0 ce1 ~$t0
      @@ addi ~$a0 ~$t0 4
      @@ li ~$v0 9
      @@ syscall
      @@ sw ~$a0 0 ~$v0
      @@ (match find_alloc id with
          | Stack o -> sw ~$v0 o ~$fp
          | Reg r1 -> move r1 v0)



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

    let args, index = List.fold_left
        (fun (v,i) arg -> match find_alloc arg with
           | Reg r -> (v @@ (if i > 3 then lw r ((i-4)*4 +8) ~$fp
                             else move r ("$a"^string_of_int(i))) ,i+1)
           | Stack o -> (v @@ (if i > 3
                               then lw ~$t0 ((i-4)*4 +8) ~$fp@@ sw ~$t0 o ~$fp
                               else sw ("$a"^string_of_int(i)) o ~$fp ) ,i+1) )
        (nop,0) p.formals
    in
    args
  in

  let store_res =
    if Symb_Tbl.mem "result" p.locals
    then
      match find_alloc "result" with
      | Reg r -> move v0 r
      | Stack o -> lw v0 o ~$fp
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
  @@ move a0 v0
  @@ li ~$a0 8
  @@ li ~$v0 9
  @@ syscall
  @@ la ~$t0 "gestion_catch"
  @@ sw ~$t0 4 ~$v0
  @@ sw ~$v0 0 ~$v0
  @@ move ~$s0 ~$v0
  @@ jal "main_integer"
  @@ label "gestion_catch"

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
