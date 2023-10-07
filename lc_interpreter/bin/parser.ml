open Batteries
open Utilities
open Lexer

module Expr = struct
  type t' =
    | EIntLit of int
    | ELet of { name : ident; value : t; body : t }
    | EFun of { arg : ident; body : t }
    | EIdent of ident
    | EBinOp of { op : bin_op Span.spanned; lhs : t; rhs : t }
    | EApp of { func : t; arg : t }

  and t = t' Span.spanned
  and ident = string Span.spanned
  and bin_op = BAdd | BSub | BMul | BDiv | BMod

  let rec expr_to_string ?(level = 0) { Span.span; node } =
    let indent = String.repeat "  " level in
    Printf.sprintf "%s @ %s\n  %s%s" (fmt_name node) (Span.to_string span)
      indent
      (fmt_contents (level + 1) node)

  and fmt_name = function
    | EIntLit _ -> "IntLit"
    | ELet _ -> "Let"
    | EFun _ -> "Fun"
    | EIdent _ -> "Ident"
    | EBinOp _ -> "BinOp"
    | EApp _ -> "App"

  and fmt_contents level =
    let indent = String.repeat "  " level in
    function
    | EIntLit num -> Int.to_string num
    | ELet { name; value; body } ->
        Printf.sprintf "name: %s\n%svalue: %s\n%sbody: %s\n" (fmt_ident name)
          indent
          (expr_to_string ~level value)
          indent
          (expr_to_string ~level body)
    | EFun { arg; body } ->
        Printf.sprintf "arg: %s\n%sbody: %s\n" (fmt_ident arg) indent
          (expr_to_string ~level body)
    | EIdent name -> fmt_ident name
    | EBinOp { op; lhs; rhs } ->
        Printf.sprintf "op: %s\n%slhs: %s\n%srhs: %s\n" (Span.fmt fmt_ops op)
          indent
          (expr_to_string ~level lhs)
          indent
          (expr_to_string ~level rhs)
    | EApp { func; arg } ->
        Printf.sprintf "func: %s\n%sarg: %s\n"
          (expr_to_string ~level func)
          indent
          (expr_to_string ~level arg)

  and fmt_ident = Span.fmt (fun ident -> "'" ^ ident ^ "'")

  and fmt_ops = function
    | BAdd -> "Add"
    | BSub -> "Sub"
    | BMul -> "Mul"
    | BDiv -> "Div"
    | BMod -> "Mod"
end

module Parser = struct
  type input = Token.t Enum.t
  type error = UnexpectedToken of Token.token_kind * Span.t

  let error_to_string _ = failwith "unimplemented"

  type 'res t = { run : input -> input * ('res, error) result }

  let expr_parser = undefined "unimplemented"
  let run p str = string_to_tokens str |> p.run |> Tuple2.second
end
