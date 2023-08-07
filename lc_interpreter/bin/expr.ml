open Utilities

type expr' =
  | EIntLit of int
  | ELet of { name : ident; body : expr }
  | EFun of { args : ident list; body : expr }
  | EIdent of ident
  | EBinOp of { op : bin_op Span.spanned; lhs : expr }
  | EApp of { func : expr; arg : expr }

and expr = expr' Span.spanned
and ident = string Span.spanned
and bin_op = Add | Sub | Mul | Div | Mod

let expr_to_string _expr = failwith "unimplemented"
