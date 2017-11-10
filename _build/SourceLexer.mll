{

  open Lexing
  open SourceParser

  exception Error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	(*"main",     MAIN;*)
        "var",      VAR;
        "integer",  INT;
        "boolean",  BOOLEAN;
        "while",    WHILE;
        "for",      FOR;
      	"print",    PRINT;
        "if",       IF;
        (*"then",     THEN;*)
        "else",     ELSE;
        "true",     TRUE;
        "false",    FALSE;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | [' ' '\t' '\r']+
      { token lexbuf }
  | '\n'
      { new_line lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | digit+
      { LITINT (int_of_string (lexeme lexbuf)) }
  | "("
      { BEGIN }
  | ")"
      { END }
  | "["
      { BB }
  | "]"
      { EB }
  | ";"
      { SEMI }
  | ","
      { COMMA }
  | "+"
      { ADD }
  | "++"
      { INC }
  | "-"
      { SUB }
  | "--"
      { DEC }
  | "*"
      { MULT }
  | "/"
      { DIV }
  | "=="
      { EQ }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | ">"
      { MT }
  | ">="
      { ME }
  | "&&"
      { AND }
  | "||"
      { OR }
  | ":="
      { SET }
  | eof
      { EOF }
  | _
      {
        let start_p = lexeme_start_p lexbuf in
        raise (Error ("Unknow char(s) \""
                         ^ (lexeme lexbuf)
                         ^ "\" in "
                         ^ start_p.pos_fname (* /!\ j'arrive pas à recup nom fichier *)
                         ^ " at line "
                         ^ (string_of_int start_p.pos_lnum)
                         ^ ", col "
                         ^ (string_of_int (start_p.pos_cnum - start_p.pos_bol))))}

and comment = parse
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "Unterminated comment" }
