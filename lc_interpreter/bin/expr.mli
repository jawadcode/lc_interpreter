open Utilities

type expr' =
  | EIntLit of int
  | ELet of { name : ident; body : expr }
  | EFun of { args : ident list; body : expr }
  | EIdent of ident
  | EBinOp of { op : bin_op Span.spanned; lhs : expr }
  | EApp of { func : expr; arg : expr }

and expr
and ident
and bin_op = Add | Sub | Mul | Div | Mod

val expr_to_string : expr -> string
