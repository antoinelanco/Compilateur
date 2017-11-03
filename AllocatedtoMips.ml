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
    | FunCall(i,s,v) -> failwith "A completer"
    | ProcCall(s,v) -> failwith "A completer"
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



    (*2.2.1. Méthode ad hoc qui ne marche pas je ne sais pas pk  *)

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

  let start_fun =
    nop
  in

  let end_fun =
    nop
  in

start_fun @@ (generate_block p.code) @@ end_fun

let init =
    move fp sp
    @@ addi fp fp (-4)
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ sw v0 0 fp
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
