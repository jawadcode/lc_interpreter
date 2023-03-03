open Batteries
open Utilities

module Token = struct
  [@@@ocaml.warning "-37"]

  type t = { span : Span.t; kind : token_kind }

  and token_kind =
    | TKLetKw
    | TKInKw
    | TKFunKw
    | TKIntLit
    | TKIdent
    | TKFatArrow
    | TKEquals
    | TKLParen
    | TKRParen
    | TKAdd
    | TKSub
    | TKMul
    | TKDiv
    | TKEof
    | TKError

  let rec to_string { span; kind } =
    string_of_token_kind kind ^ " @ " ^ Span.to_string span

  and string_of_token_kind = function
    | TKLetKw -> "LetKw"
    | TKInKw -> "InKw"
    | TKFunKw -> "FunKw"
    | TKIntLit -> "IntLit"
    | TKIdent -> "Ident"
    | TKFatArrow -> "FatArrow"
    | TKEquals -> "Equals"
    | TKLParen -> "LParen"
    | TKRParen -> "RParen"
    | TKAdd -> "Add"
    | TKSub -> "Sub"
    | TKMul -> "Mul"
    | TKDiv -> "Div"
    | TKEof -> "Eof"
    | TKError -> "Error"
end

[@@@ocaml.warning "-69"]
type lexer = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable eof : bool;
}

let rec enum_of_string source =
  let lexer = { source; start = 0; current = 0; eof = false } in
  Enum.from_while (next_token lexer)

and next_token lexer _ =
  if lexer.eof then None
  else
    let kind = next_kind lexer in
    if kind = Token.TKEof then lexer.eof <- true;
    Some { Token.span = { start = lexer.start; finish = lexer.current }; kind }

and next_kind _lexer = TKEof
