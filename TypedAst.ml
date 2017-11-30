module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_info = SourceAst.identifier_info
type identifier_kind = SourceAst.identifier_kind
type literal         = SourceAst.literal
type binop           = SourceAst.binop
type prog            = SourceAst.prog
type typ             = SourceAst.typ

type ('a, 'e) annotated_element = { annot: 'a ; elt:   'e }

and typed_expression =  (typ, expression) annotated_element
and typed_location   =  (typ, location) annotated_element
and typed_call       = (typ option, call) annotated_element

and call = string * typed_expression list

and block = instruction list

and function_info = {
  return:  typ option;
  formals: (typ * string) list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block
}

and instruction =
  | ProcCall  of typed_call
  | Set       of typed_location   * typed_expression    (* Affectation *)
  | While     of typed_expression * block         (* Boucle      *)
  | If        of typed_expression * block * block (* Branchement *)
  | Print     of typed_expression                 (* Affichage   *)

and expression =
  | FunCall   of typed_call
  | Literal   of literal                         (* Valeur immédiate   *)
  | Location  of typed_location                        (* Valeur en mémoire  *)
  | Binop     of binop * typed_expression * typed_expression (* Opération binaire  *)
  | NewArray  of typed_expression * typ
  | NewArrayAcol of typed_expression list

and location =
  | ArrayAccess of typed_expression * typed_expression
  | Identifier  of string (* Variable en mémoire *)
