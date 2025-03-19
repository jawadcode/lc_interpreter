open Batteries
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

let expr_parser =
  let open Parser_combis in
  just Token.TKIntLit
  |> map
       (Span.map_source (fun source ->
            EIntLit (Substring.to_string source |> Int.of_string)))

let rec expr_to_string ?(level = 0) expr =
  let indent = String.repeat "  " level in
  Printf.sprintf "%s\n  %s%s" (Span.fmt fmt_name expr) indent
    (fmt_contents (level + 1) expr.node)

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
