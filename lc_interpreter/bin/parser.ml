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
end

module Parser = struct
  type input = Token.t Enum.t

  type error =
    | UnexpectedToken of { got : Token.t; expected : string }
    | UnexpectedEndOfInput

  let error_to_string = function
    | UnexpectedToken { got = { span; source; node = _ }; expected } ->
        Printf.sprintf "Syntax Error [%d-%d]: Expected %s, got '%s'" span.start
          span.finish expected
          (Substring.to_string source)
    | UnexpectedEndOfInput -> "Syntax Error: Unexpected end of input"

  type 'res t = { run : input -> input * ('res, error) result }

  let return x = { run = (fun input -> (input, Ok x)) }
  let fail err : 'a t = { run = (fun input -> (input, Error err)) }

  let just tk =
    {
      run =
        (fun input ->
          ( input,
            match Enum.get input with
            | Some token2 when token2.node == tk -> Ok token2
            | Some token2 ->
                Error
                  (UnexpectedToken
                     { got = token2; expected = Token.token_kind_to_string tk })
            | None -> Error UnexpectedEndOfInput ));
    }

  let map f parser =
    {
      run =
        (function
          | input, Ok x -> (input, Ok (f x))
          | input, Error err -> (input, Error err))
        % parser.run;
    }

  let bind f parser =
    {
      run =
        (function
          | input, Ok x -> (f x).run input
          | input, Error err -> (input, Error err))
        % parser.run;
    }

  let ( *> ) parser1 parser2 =
    {
      run =
        (fun input ->
          let input, res = parser1.run input in
          match res with
          | Ok _ -> parser2.run input
          | Error err -> (input, Error err));
    }

  let ( <* ) parser1 parser2 =
    {
      run =
        (fun input ->
          let input, res = parser1.run input in
          match res with
          | Ok x -> (
              let input, res = parser2.run input in
              match res with
              | Ok _ -> (input, Ok x)
              | Error err -> (input, Error err))
          | Error err -> (input, Error err));
    }

  let ( <*> ) parser1 parser2 =
    {
      run =
        (fun input ->
          let input, res = parser1.run input in
          match res with
          | Ok x -> (
              let input, res = parser2.run input in
              match res with
              | Ok y -> (input, Ok (x, y))
              | Error err -> (input, Error err))
          | Error err -> (input, Error err));
    }

  let ( <|> ) parser1 parser2 =
    {
      run =
        (fun input ->
          let input, res = parser1.run input in
          match res with Ok x -> (input, Ok x) | Error _ -> parser2.run input);
    }

  let optional parser =
    {
      run =
        (fun input ->
          let input, res = parser.run input in
          match res with
          | Ok x -> (input, Ok (Some x))
          | Error _ -> (input, Ok None));
    }

  let many parser =
    {
      run =
        (fun input ->
          let xs = ref [] in
          let rec loop input =
            let input, res = parser.run input in
            match res with
            | Ok x ->
                xs := x :: !xs;
                loop input
            | Error _ -> input
          in
          let input = loop input in
          (input, Ok (List.rev !xs)));
    }

  let expr_parser =
    just Token.TKIntLit
    |> map
         (Span.map_source (fun source ->
              Expr.EIntLit (Substring.to_string source |> Int.of_string)))

  let run p str = string_to_tokens str |> p.run |> Tuple2.second
end
