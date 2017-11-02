(* Syntaxe abstraite non typée *)
(* Cette version est obtenu en retirant tous les indications de typage *)
module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_info = SourceAst.identifier_kind

type expression  = SourceAst.expression
type location    = SourceAst.location
type block       = SourceAst.block
type instruction = SourceAst.instruction
type literal     = SourceAst.literal
type binop       = SourceAst.binop
type call        = SourceAst.call

(* Programme principal : une table de symboles et un bloc de code *)
type function_info = {
  locals: identifier_info Symb_Tbl.t;
  code:   block;
}

and prog = function_info Symb_Tbl.t
