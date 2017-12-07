exception Error

type token = 
  | WHILE
  | VAR
  | TRY
  | TRUE
  | TO
  | THROW
  | SUB
  | STRUCT
  | SET
  | SEMI
  | PRINT
  | OR
  | NEW
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
  | DOT
  | DIV
  | DEC
  | COMMA
  | CATCH
  | BOOLEAN
  | BEGIN
  | BB
  | BA
  | AND
  | ADD


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.prog)