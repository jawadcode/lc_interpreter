open Lexer

type 'a spanned = { span : source_span; node : 'a }

let ( + ) lhs rhs = { start = lhs.start; finish = rhs.finish }
let map_span f s = { s with node = f s.node }

type expr =
  | EIntLit of int
  | ELet of { name : ident; body : spanned_expr }
  | EFun of { args : ident list; body : spanned_expr }
  | EIdent of ident
  | EBinOp of { op : bin_op spanned; lhs : spanned_expr }
  | EApp of { func : spanned_expr; arg : spanned_expr }

and ident = string spanned
and spanned_expr = expr spanned
and bin_op = Add | Sub | Mul | Div