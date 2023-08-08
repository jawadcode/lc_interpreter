open Utilities
open Lexer

module Expr : sig
  type t' =
    | EIntLit of int
    | ELet of { name : ident; body : t }
    | EFun of { args : ident list; body : t }
    | EIdent of ident
    | EBinOp of { op : bin_op Span.spanned; lhs : t }
    | EApp of { func : t; arg : t }

  and t = t' Span.spanned
  and ident = string Span.spanned
  and bin_op = Add | Sub | Mul | Div | Mod

  val expr_to_string : ?level:int -> t -> string
end

module Parser : sig
  type input = Token.t BatEnum.t
  type error = UnexpectedToken of Token.token_kind * Span.t

  val error_to_string : error -> string

  type 'res t = { run : input -> input * ('res, error) result }

  val expr_parser : Expr.t t
  val run : 'a t -> string -> ('a, error) result
end
