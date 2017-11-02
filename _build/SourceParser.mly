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
%token WHILE FOR
%token INT BOOLEAN
%token IF THEN ELSE
%token TRUE FALSE
%token INC DEC
%token ADD MULT SUB DIV EQ NEQ LT LE MT ME AND OR SET
%token EOF

%left AND OR
%left EQ NEQ LT LE MT ME
%left ADD SUB
%left MULT DIV

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
| (* empty *) EOF { Symb_Tbl.empty }
| fs=fun_decl; p=prog
    {
      let (id, infos) = fs in
      Symb_Tbl.add id infos p
    }

typs:
| INT { TypInteger }
| BOOLEAN { TypBoolean }

var_decls:
| (* empty *) { Symb_Tbl.empty }
| VAR; t=typs; id=IDENT; SEMI; tbl=var_decls { let info = {typ=t; kind=Local} in Symb_Tbl.add id info tbl }

fun_decl:
| t=typs; id=IDENT; BEGIN; ps=para; END; BEGIN; vds=var_decls; is=instructions; END {

  let merge_vars k v1 v2 =
      match v1, v2 with
      |Some v1,_ -> Some v1
      |None,Some v2 -> Some v2
      |None,None -> None
      in

    let res  = Symb_Tbl.singleton "result" { typ=t; kind=Return } in

    let local = Symb_Tbl.merge merge_vars res vds in
    let index = ref 0 in
    let ftl = List.fold_left (fun acc (t,i) ->
      incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
      Symb_Tbl.empty ps in

    let locals = Symb_Tbl.merge merge_vars local ftl in


    let formals = List.fold_left (fun acc (t,_) -> t::acc) [] ps in




    id, {
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

      let formals = List.fold_left (fun acc (t,_) -> t::acc) [] ps in
      id, {
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
| c=call { [ProcCall(c)] }
| PRINT; BEGIN; e=expression; END         { [Print(e)] }
| id=IDENT; SET; e=expression { [Set(Identifier id,e)] }
| id=IDENT; INC { [Set(Identifier id,Binop(Add,Location( Identifier id ),Literal(Int 1)) )] }
| id=IDENT; DEC { [Set(Identifier id,Binop(Sub,Location( Identifier id ),Literal(Int 1)) )] }
| WHILE; e=expression; BEGIN; is=instructions; END { [While(e,is)] }
| IF; e=expression; THEN; BEGIN; is1=instructions; END; ELSE; BEGIN; is2=instructions; END; { [If(e,is1,is2)] }
| IF; e=expression; THEN; BEGIN; is=instructions; END { [If(e,is,[])] }
| FOR; id1=IDENT; SET; e1=expression; SEMI; e2=expression; SEMI; id2=IDENT; SET; e3=expression; BEGIN; bl=instructions; END
    { let block = bl @ [Set(Identifier id2, e3)] in [Set(Identifier id1, e1);While(e2, block)] }

expression:
| c=call { FunCall(c) }
| id=IDENT { Location( Identifier id ) }
| i=LITINT { Literal(Int i) }
| TRUE { Literal(Bool true) }
| FALSE { Literal(Bool false) }
| e1=expression; b=binop; e2=expression   { Binop(b,e1,e2) }
;

call:
| id=IDENT; BEGIN; a=arg; END { id, a }

arg:
| (* empty *) { [] }
| a=arguments { a }

arguments:
| e=expression { [e] }
| e=expression; COMMA ; a=arguments { e :: a }



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
