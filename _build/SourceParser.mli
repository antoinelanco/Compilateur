exception Error

type token = 
  | WHILE
  | VAR
  | TRUE
  | TO
  | SUB
  | SET
  | SEMI
  | PRINT
  | OR
  | NEQ
  | MULT
  | MT
  | ME
  | LT
  | LITINT of (int)
  | LE
  | INT
  | INC
  | IF
  | IDENT of (string)
  | FOR
  | FALSE
  | EQ
  | EOF
  | END
  | ELSE
  | EB
  | EA
  | DIV
  | DEC
  | COMMA
  | BOOLEAN
  | BEGIN
  | BB
  | BA
  | AND
  | ADD


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.prog)