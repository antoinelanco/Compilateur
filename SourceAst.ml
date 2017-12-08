(* Syntaxe abstraite typée *)

module Symb_Tbl = Map.Make(String)

(* Programme principal : une table de symboles et un bloc de code *)
(* type main = {
   locals: identifier_info Symb_Tbl.t;
   code: block;
   } *)

type prog = {
  functions : function_info Symb_Tbl.t;
  structs : struct_info Symb_Tbl.t
}
and struct_info = (string * typ) list

and function_info = {
  return:  typ option;
  formals: (typ * string) list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block
}

and f_access = expression * string

and call = string * expression list

(* La table des symboles contient, pour chaque variable :
   - sa nature  : variable locale ou paramètre formel
   - son type : entier ou booléen
*)
and identifier_kind =
  | Local   (* Variable locale    *)
  | Formal of int (* Paramètre formel  *)
  | Return
and identifier_info = { typ: typ; kind: identifier_kind }
and typ =
  | TypStruct of string
  | TypInteger
  | TypBoolean
  | TypArray of typ

(* Un bloc de code est une liste d'instructions *)
and block = (Lexing.position * instruction) list
and instruction =
  | Throw
  | Try       of block * block
  | ProcCall  of call
  | Set       of location   * expression    (* Affectation *)
  | While     of expression * block         (* Boucle      *)
  | If        of expression * block * block (* Branchement *)
  | Print     of expression                 (* Affichage   *)

and expression =
  | NewRecord of string
  | FunCall   of call
  | Literal   of literal                         (* Valeur immédiate   *)
  | Location  of location                        (* Valeur en mémoire  *)
  | Binop     of binop * expression * expression (* Opération binaire  *)
  | NewArray  of expression * typ
  | NewArrayAcol of expression list

and literal =
  | Int  of int  (* Constante entière   *)
  | Bool of bool (* Constante booléenne *)

and location =
  | FieldAccess of f_access
  | ArrayAccess of expression * expression
  | Identifier  of string (* Variable en mémoire *)

and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *) | Div (* / *)
  | Eq  (* == *) | Neq  (* != *)
  | Lt  (* <  *) | Le   (* <= *) | Mt (* > *) | Me (* >= *)
  | And (* && *) | Or   (* || *)


(* Cadeau pour le débogage : un afficheur.
   [print_main m] produit une chaîne de caractère représentant le programme
*)
open Printf

let rec print_typ = function
  | TypInteger -> "integer"
  | TypBoolean -> "boolean"
  | TypArray t -> sprintf "tableau_de_%s" (print_typ t)
  | TypStruct n -> sprintf "struct_de_%s" n

let print_identifier_info i = print_typ i.typ

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
      (sprintf "  var %s %s;\n" (print_identifier_info i) v) ^ s
    ) tbl ""

let rec print_literal = function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
and print_location = function
  | Identifier x -> x
  | FieldAccess (e,id) -> sprintf "%s.%s" (print_expression e) id
  | ArrayAccess (id,e) -> sprintf "%s[%s]" (print_expression id) (print_expression e)
and print_binop = function
  | Add  -> "+"
  | Mult -> "*"
  | Sub  -> "-"
  | Div  -> "/"
  | Eq   -> "=="
  | Neq  -> "!="
  | Lt   -> "<"
  | Le   -> "<="
  | Mt   -> ">"
  | Me   -> ">="
  | And  -> "&&"
  | Or   -> "||"
and print_expression = function
  | NewRecord(id) -> sprintf "new struct %s" id
  | NewArrayAcol(es) -> let var_tab = List.fold_left (fun acc i -> (print_expression i)^"; "^acc) "" es in
    sprintf "Creation d'un tableau : {"^var_tab^"}"
  | NewArray(e, t) -> sprintf "Creation d'un tableau de %s de taille %s" (print_typ t) (print_expression e)
  | FunCall c -> "FunCall" (*A completer*)
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expression e1) (print_binop op) (print_expression e2)

let offset o = String.make (2*o) ' '
let rec print_block o = function
  | [] -> ""
  | (pos,i)::b -> (offset o) ^ (print_instruction pos o i) ^ ";\n" ^ (print_block o b)
and print_instruction pos o = function
  | Throw -> "Throw"
  | Try(b1,b2) -> sprintf "Try (\n%s%s) Catch (\n%s%s)"
                    (print_block (o+1) b1) (offset o)
                    (print_block (o+1) b2) (offset o)
  | ProcCall(s,el) -> sprintf "%s(args)" s (*A completer*)
  | Set(id, e) -> sprintf "%s := %s" (print_location id) (print_expression e)
  | While(e, b) ->
    sprintf "while %s (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b) (offset o)
  | If(e, b1, b2) ->
    sprintf "if %s then (\n%s%s) else (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b1) (offset o)
      (print_block (o+1) b2) (offset o)
  | Print(e) -> sprintf "print(%s)" (print_expression e)

let print_main m =
  sprintf "main(int x) (\n%s%s)\n"
    (print_symb_tbl m.locals) (print_block 1 m.code)
