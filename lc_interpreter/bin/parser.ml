open Batteries
open Utilities
open Lexer

module Expr = struct
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

  let rec expr_to_string ?(level = 0) { Span.span; node } =
    let indent = String.repeat "  " level in
    indent ^ fmt_name node ^ " @ " ^ Span.to_string span ^ "\n" ^ "  " ^ indent
    ^ fmt_contents node

  and fmt_name = function
    | EIntLit _ -> "IntLit"
    | ELet _ -> "Let"
    | EFun _ -> "Fun"
    | EIdent _ -> "Ident"
    | EBinOp _ -> "BinOp"
    | EApp _ -> "App"

  and fmt_contents = function
    | EIntLit num -> ""
    | ELet { name; body } -> ""
    | EFun { args; body } -> ""
    | EIdent name -> ""
    | EBinOp { op = { Span.span; node = op }; lhs } -> ""
    | EApp { func; arg } -> ""
end

module Parser = struct
  type input = Token.t Enum.t
  type error = UnexpectedToken of Token.token_kind * Span.t

  let error_to_string = failwith "unimplemented"

  type 'res t = { run : input -> input * ('res, error) result }

  let expr_parser = failwith "unimplemented"
  let run p str = string_to_tokens str |> p.run |> Tuple2.second
end
