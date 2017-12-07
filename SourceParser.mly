%{

  open SourceAst

%}

%token <string> IDENT
%token BEGIN END
%token SEMI COMMA
%token <int> LITINT
/*%token MAIN*/
%token VAR
%token PRINT
%token WHILE FOR TO
%token INT BOOLEAN
%token IF ELSE /*THEN*/
%token TRUE FALSE
%token INC DEC
%token ADD MULT SUB DIV EQ NEQ LT LE MT ME AND OR SET
%token EOF
%token BB EB BA EA
%token NEW STRUCT DOT
%token CATCH TRY THROW


%left AND OR
%left EQ NEQ LT LE MT ME
%left ADD SUB
%left MULT DIV
%left BB
%left DOT

/*%start main
%type <SourceAst.main> main*/

%start prog
%type <SourceAst.prog> prog

%%


/*main:
| MAIN; BEGIN; INT; x=IDENT; END;
  BEGIN; vds=var_decls; is=instructions; END; EOF  {

    let infox = { typ=TypInteger; kind=FormalX } in
    let init  = Symb_Tbl.singleton x infox in

    let merge_vars k v1 v2 =
        match v1, v2 with
        |Some v1,_ -> Some v1
        |None,Some v2 -> Some v2
        |None,None -> None
        in
  let locals = Symb_Tbl.merge merge_vars init vds in
    {locals = locals; code=is} }
;*/


prog:
| s=structs; p=progs; EOF
  { {functions = p; structs = s} }

progs:
| (* empty *) { Symb_Tbl.empty }
| fs=fun_decl; p=progs
    {
      let (id, infos) = fs in
      Symb_Tbl.add id infos p
    }

structs:
| (* empty *) { Symb_Tbl.empty }
| st=struct_decl; sts=structs
  {
    let (id,infos) = st in
    Symb_Tbl.add id infos sts
  }


struct_decl:
| STRUCT; id=IDENT; BEGIN; fds=separated_list(SEMI, field_decl); END
  {id,fds}

field_decl:
| t=typs id=IDENT {(id,t)}

typs:
| INT { TypInteger }
| BOOLEAN { TypBoolean }
| BB; EB; t=typs { TypArray(t) }
| BEGIN; id=IDENT; END { TypStruct(id) }

var_decls:
| (* empty *) { Symb_Tbl.empty }
| VAR; t=typs; id=IDENT; SEMI; tbl=var_decls
  { let info = {typ=t; kind=Local} in Symb_Tbl.add id info tbl }

fun_decl:
| t=typs; id=IDENT; BEGIN; ps=para; END; BEGIN; vds=var_decls; is=instructions; END {

  let merge_vars k v1 v2 =
      match v1, v2 with
      |Some v1,_ -> Some v1
      |None,Some v2 -> Some v2
      |None,None -> None
      in


    let index = ref 0 in
    let ftl = List.fold_left (fun acc (t,i) ->
      incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
      Symb_Tbl.empty ps in

    let locals = Symb_Tbl.merge merge_vars vds ftl in
    let locals = Symb_Tbl.add "result" { typ=t; kind=Return } locals in
    let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in
    let new_id =
      List.fold_left (fun acc (t,_) -> acc^"_"^(SourceAst.print_typ t))
        id ps
    in

    new_id, {
      return=Some t;
      formals=formals;
      locals=locals;
      code=is
    }
  }

  | id=IDENT; BEGIN; ps=para; END; BEGIN; vds=var_decls; is=instructions; END {

      let merge_vars k v1 v2 =
          match v1, v2 with
          |Some v1,_ -> Some v1
          |None,Some v2 -> Some v2
          |None,None -> None
          in

      let index = ref 0 in
      let ftl = List.fold_left (fun acc (t,i) ->
        incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
        Symb_Tbl.empty ps in
      let locals = Symb_Tbl.merge merge_vars vds ftl in
      let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in
      let new_id =
        List.fold_left (fun acc (t,_) -> acc^"_"^(SourceAst.print_typ t))
          id ps
      in
      new_id, {
        return=None;
        formals=formals;
        locals=locals;
        code=is
      }
    }


    para:
    | (* empty *) { [] }
    | p=parameters { p }

    parameters:
    | t=typs; id=IDENT { [(t,id)] }
    | t=typs; id=IDENT; COMMA; p=parameters { (t,id) :: p }



instructions:
| (* empty *)                             { [] }
| i=instruction; SEMI; is=instructions    { i @ is }

instruction:
| THROW
  { [($startpos,Throw)] }
| TRY; BEGIN; is1=instructions; END; CATCH; BEGIN; is2=instructions; END
  { [($startpos,Try(is1,is2))] }
| c=call
  { [($startpos,ProcCall(c))] }
| PRINT; BEGIN; e=expression; END
  { [($startpos,Print(e))] }
| s=set_direct
  { s }
| l=location; SET; e=expression
  { [($startpos,Set(l,e))] }
| WHILE; BEGIN; e=expression; END; BEGIN; is=instructions; END
  { [($startpos,While(e,is))] }
| IF; BEGIN; e=expression; END; BEGIN; is1=instructions; END; ELSE; BEGIN; is2=instructions; END;
  { [($startpos,If(e,is1,is2))] }
| IF; BEGIN; e=expression; END; BEGIN; is=instructions; END
  { [($startpos,If(e,is,[]))] }

| FOR; BEGIN; l1=IDENT; SET; e1=expression; SEMI; e2=expression; SEMI;
  l3=IDENT; SET; e3=expression; END; BEGIN; bl=instructions; END
  { let block = bl @ [($startpos,Set(Identifier(l3),e3))] in
     [($startpos,Set(Identifier(l1),e1));($startpos,While(e2, block))] }

| FOR; BEGIN; l1=IDENT; SET; e1=expression; SEMI; e2=expression; SEMI;
  s=set_direct; END; BEGIN; bl=instructions; END
  { let block = bl @ s in
    [($startpos,Set(Identifier(l1),e1));($startpos,While(e2, block))] }

| FOR; BEGIN; id=IDENT; SET; i1=literal; TO; i2=literal; END; BEGIN; bl=instructions; END;
  { let block = bl @ [($startpos,Set(Identifier id, Binop(Add,Location( Identifier id ),Literal(Int 1))))] in
     [($startpos,Set(Identifier id, Literal i1));
     ($startpos,While(Binop(Le,Location(Identifier id),Literal i2), block))] }


set_direct:
| id=IDENT; INC
  { [($startpos,Set(Identifier id,Binop(Add,Location( Identifier id ),Literal(Int 1)) ))] }
| id=IDENT; DEC
  { [($startpos,Set(Identifier id,Binop(Sub,Location( Identifier id ),Literal(Int 1))) )] }


expression:
| c=call { FunCall(c) }
| NEW; id=IDENT; BEGIN; END { NewRecord(id) }
| BA; es=separated_list(SEMI, expression); EA { NewArrayAcol(es) }
| BB; e=expression; EB; t=typs { NewArray(e,TypArray t) }
| loc=location { Location(loc) }
| i=literal { Literal(i) }
| e1=expression; b=binop; e2=expression   { Binop(b,e1,e2) }

literal:
| i=LITINT { Int i }
| TRUE { Bool true }
| FALSE { Bool false }

location:
| e=expression; DOT; id=IDENT { FieldAccess(e,id) }
| id=IDENT { Identifier(id) }
| e1=expression; BB; e=expression; EB { ArrayAccess(e1,e) }



call:
| id=IDENT; BEGIN; a=arg; END { id, a }


arg:
| (* empty *) { [] }
| a=arguments { a }

arguments:
| e=expression { [e] }
| e=expression; COMMA ; a=arguments { a @ [e] }



%inline binop:

| MULT { Mult }
| DIV  { Div }
| ADD  { Add }
| SUB  { Sub }
| LT   { Lt }
| LE   { Le }
| MT   { Mt }
| ME   { Me }
| EQ   { Eq }
| NEQ  { Neq }
| AND  { And }
| OR   { Or }
