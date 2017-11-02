exception Error

type token = 
  | WHILE
  | VAR
  | TRUE
  | THEN
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
  | DIV
  | DEC
  | COMMA
  | BOOLEAN
  | BEGIN
  | AND
  | ADD


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.prog)