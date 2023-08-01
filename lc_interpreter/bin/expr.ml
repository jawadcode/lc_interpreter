open Lexer

type 'a spanned = { span : source_span; node : 'a }

let ( + ) lhs rhs = { start = lhs.start; finish = rhs.finish }
let map_span f s = { s with node = f s.node }

type expr' =
  | EIntLit of int
  | ELet of { name : ident; body : expr }
  | EFun of { args : ident list; body : expr }
  | EIdent of ident
  | EBinOp of { op : bin_op spanned; lhs : expr }
  | EApp of { func : expr; arg : expr }

and expr = expr' spanned
and ident = string spanned
and bin_op = Add | Sub | Mul | Div
