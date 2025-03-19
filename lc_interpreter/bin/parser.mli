open Utilities
open Lexer

type expr' =
  | EIntLit of int
  | ELet of { name : ident; value : expr; body : expr }
  | EFun of { arg : ident; body : expr }
  | EIdent of ident
  | EBinOp of { op : bin_op Span.spanned; lhs : expr; rhs : expr }
  | EApp of { func : expr; arg : expr }

and expr = expr' Span.spanned
and ident = string Span.spanned
and bin_op = BAdd | BSub | BMul | BDiv | BMod

val expr_parser : (Token.token_kind, expr) Parser_combis.t
val expr_to_string : ?level:int -> expr -> string
