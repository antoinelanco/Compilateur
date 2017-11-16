# 1 "SourceLexer.mll"
 

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
        "to",       TO;
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


# 32 "SourceLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\229\255\230\255\002\000\001\000\001\000\003\000\004\000\
    \005\000\006\000\240\255\241\255\001\000\025\000\246\255\247\255\
    \248\255\249\255\250\255\251\255\021\000\084\000\254\255\002\000\
    \244\255\242\255\239\255\238\255\236\255\234\255\233\255\232\255\
    \231\255\039\000\252\255\253\255\039\000\040\000\255\255\254\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\026\000\026\000\026\000\020\000\018\000\
    \026\000\026\000\255\255\255\255\012\000\010\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\002\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\002\000\002\000\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\000\000\000\000\255\255\255\255\000\000\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\022\000\023\000\000\000\023\000\000\000\023\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\008\000\023\000\000\000\000\000\000\000\005\000\030\000\
    \019\000\018\000\011\000\013\000\014\000\012\000\025\000\010\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\003\000\015\000\007\000\009\000\006\000\032\000\
    \029\000\028\000\027\000\026\000\024\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\037\000\
    \039\000\036\000\038\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\017\000\000\000\016\000\000\000\021\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\004\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\000\000\
    \000\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\023\000\255\255\000\000\255\255\023\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\023\000\255\255\255\255\255\255\000\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \006\000\007\000\008\000\009\000\013\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\033\000\
    \036\000\033\000\037\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\021\000\000\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\255\255\
    \255\255\255\255\255\255\021\000\255\255\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\033\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 37 "SourceLexer.mll"
      ( token lexbuf )
# 164 "SourceLexer.ml"

  | 1 ->
# 39 "SourceLexer.mll"
      ( new_line lexbuf; token lexbuf )
# 169 "SourceLexer.ml"

  | 2 ->
# 41 "SourceLexer.mll"
      ( id_or_keyword (lexeme lexbuf) )
# 174 "SourceLexer.ml"

  | 3 ->
# 43 "SourceLexer.mll"
      ( LITINT (int_of_string (lexeme lexbuf)) )
# 179 "SourceLexer.ml"

  | 4 ->
# 45 "SourceLexer.mll"
      ( BEGIN )
# 184 "SourceLexer.ml"

  | 5 ->
# 47 "SourceLexer.mll"
      ( END )
# 189 "SourceLexer.ml"

  | 6 ->
# 49 "SourceLexer.mll"
      ( BB )
# 194 "SourceLexer.ml"

  | 7 ->
# 51 "SourceLexer.mll"
      ( EB )
# 199 "SourceLexer.ml"

  | 8 ->
# 53 "SourceLexer.mll"
      ( SEMI )
# 204 "SourceLexer.ml"

  | 9 ->
# 55 "SourceLexer.mll"
      ( COMMA )
# 209 "SourceLexer.ml"

  | 10 ->
# 57 "SourceLexer.mll"
      ( ADD )
# 214 "SourceLexer.ml"

  | 11 ->
# 59 "SourceLexer.mll"
      ( INC )
# 219 "SourceLexer.ml"

  | 12 ->
# 61 "SourceLexer.mll"
      ( SUB )
# 224 "SourceLexer.ml"

  | 13 ->
# 63 "SourceLexer.mll"
      ( DEC )
# 229 "SourceLexer.ml"

  | 14 ->
# 65 "SourceLexer.mll"
      ( MULT )
# 234 "SourceLexer.ml"

  | 15 ->
# 67 "SourceLexer.mll"
      ( DIV )
# 239 "SourceLexer.ml"

  | 16 ->
# 69 "SourceLexer.mll"
      ( EQ )
# 244 "SourceLexer.ml"

  | 17 ->
# 71 "SourceLexer.mll"
      ( NEQ )
# 249 "SourceLexer.ml"

  | 18 ->
# 73 "SourceLexer.mll"
      ( LT )
# 254 "SourceLexer.ml"

  | 19 ->
# 75 "SourceLexer.mll"
      ( LE )
# 259 "SourceLexer.ml"

  | 20 ->
# 77 "SourceLexer.mll"
      ( MT )
# 264 "SourceLexer.ml"

  | 21 ->
# 79 "SourceLexer.mll"
      ( ME )
# 269 "SourceLexer.ml"

  | 22 ->
# 81 "SourceLexer.mll"
      ( AND )
# 274 "SourceLexer.ml"

  | 23 ->
# 83 "SourceLexer.mll"
      ( OR )
# 279 "SourceLexer.ml"

  | 24 ->
# 85 "SourceLexer.mll"
      ( SET )
# 284 "SourceLexer.ml"

  | 25 ->
# 87 "SourceLexer.mll"
      ( EOF )
# 289 "SourceLexer.ml"

  | 26 ->
# 89 "SourceLexer.mll"
      (
        let start_p = lexeme_start_p lexbuf in
        raise (Error ("Unknow char(s) \""
                         ^ (lexeme lexbuf)
                         ^ "\" in "
                         ^ start_p.pos_fname (* /!\ j'arrive pas à recup nom fichier *)
                         ^ " at line "
                         ^ (string_of_int start_p.pos_lnum)
                         ^ ", col "
                         ^ (string_of_int (start_p.pos_cnum - start_p.pos_bol)))))
# 303 "SourceLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 33
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 102 "SourceLexer.mll"
      ( comment lexbuf; comment lexbuf )
# 315 "SourceLexer.ml"

  | 1 ->
# 104 "SourceLexer.mll"
      ( () )
# 320 "SourceLexer.ml"

  | 2 ->
# 106 "SourceLexer.mll"
      ( comment lexbuf )
# 325 "SourceLexer.ml"

  | 3 ->
# 108 "SourceLexer.mll"
      ( failwith "Unterminated comment" )
# 330 "SourceLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

