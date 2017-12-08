
module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_info = SourceAst.identifier_kind
type binop = SourceAst.binop
type block = instruction list

and instruction =
  | Throw
  | Try   of block * block 
  | Print of expression
  | Set   of location   * expression    (* Affectation *)
  | While of expression * block         (* Boucle      *)
  | If    of expression * block * block (* Branchement *)
  | ProcCall of call

and expression =
  | Literal   of literal      (* Valeur immédiate   *)
  | Location  of location   (* Valeur en mémoire  *)
  | Binop     of binop * expression * expression (* Opération binaire  *)
  | FunCall   of call
  | NewArray  of expression
  | NewArrayAcol of expression list
and location =
  | Identifier of string
  | ArrayAccess of expression * expression
and literal =
  | Int  of int  (* Constante entière   *)
  | Bool of bool

and call = string * expression list

and function_info = {
  formals: string list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block
}

and prog = function_info Symb_Tbl.t
