open Batteries
open Utilities

module Token = struct
  type t = token_kind Span.spanned

  and token_kind =
    | TKLetKw
    | TKInKw
    | TKFunKw
    | TKIntLit
    | TKIdent
    | TKFatArrow
    | TKBind
    | TKLParen
    | TKRParen
    | TKAdd
    | TKSub
    | TKMul
    | TKDiv
    | TKMod
    | TKEof
    | TKError

  let rec to_string (tok : t) =
    token_kind_to_string tok.node ^ " @ " ^ Span.to_string tok.span

  and token_kind_to_string = function
    | TKLetKw -> "'let'"
    | TKInKw -> "'in'"
    | TKFunKw -> "'fun'"
    | TKIntLit -> "integer literal"
    | TKIdent -> "identifier"
    | TKFatArrow -> "'=>'"
    | TKBind -> "'='"
    | TKLParen -> "'('"
    | TKRParen -> "')'"
    | TKAdd -> "'+'"
    | TKSub -> "'-'"
    | TKMul -> "'*'"
    | TKDiv -> "'/'"
    | TKMod -> "'%'"
    | TKEof -> "EOF"
    | TKError -> "<invalid token>"
end

type lexer = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable eof : bool;
}

open Token
open Ops

let rec string_to_tokens source =
  let lexer = { source; start = 0; current = 0; eof = false } in
  Enum.from_while @@ next_token lexer

and next_token lexer _ =
  let _ = skip_whitespace lexer in
  lexer.start <- lexer.current;
  let+ node =
    match next_kind lexer with
    | Some kind -> Some kind
    | None when lexer.eof -> None
    | None ->
        lexer.eof <- true;
        Some TKEof
  in
  let span = lexer_span lexer in
  {
    Span.span;
    source =
      Substring.substring lexer.source span.start (span.finish - span.start);
    node;
  }

and lexer_span lexer = { Span.start = lexer.start; Span.finish = lexer.current }

and next_kind lexer =
  let+ ch = next_char lexer in
  if is_ident ch then ident lexer
  else if Char.is_digit ch then number lexer
  else
    match ch with
    | '(' -> TKLParen
    | ')' -> TKRParen
    | '+' -> TKAdd
    | '-' -> TKSub
    | '*' -> TKMul
    | '/' -> TKDiv
    | '%' -> TKMod
    | '=' -> if match_char lexer '>' then TKFatArrow else TKBind
    | _ -> TKError

and match_char lexer expected =
  match peek_char lexer with
  | Some peek'd when peek'd = expected ->
      skip lexer;
      true
  | _ -> false

and ident lexer =
  match peek_char lexer with
  | Some peek'd when is_ident peek'd || Char.is_digit peek'd ->
      skip lexer;
      ident lexer
  | _ -> ident_type lexer

and ident_type lexer =
  match lexer.source.[lexer.start] with
  | 'l' -> check_kw lexer 1 "et" TKLetKw
  | 'i' -> check_kw lexer 1 "n" TKInKw
  | 'f' -> check_kw lexer 1 "un" TKFunKw
  | _ -> TKIdent

and check_kw lexer start rest kind =
  let src_start = lexer.start + start in
  if
    lexer.current - lexer.start = start + String.length rest
    && str_eq lexer.source src_start rest
  then kind
  else TKIdent

and str_eq src src_start rest =
  String.sub src src_start @@ String.length rest = rest

and is_ident ch = Char.is_letter ch || ch = '_'

and number lexer =
  match peek_char lexer with
  | Some peek'd when Char.is_digit peek'd ->
      skip lexer;
      number lexer
  | _ -> TKIntLit

and next_char lexer =
  if is_at_end lexer then None
  else
    let ch = lexer.source.[lexer.current] in
    lexer.current <- lexer.current + 1;
    Some ch

and skip_whitespace lexer =
  let* peek'd = peek_char lexer in
  if Char.is_whitespace peek'd then (
    skip lexer;
    skip_whitespace lexer)
  else Some ()

and peek_char lexer =
  if is_at_end lexer then None else Some lexer.source.[lexer.current]

and is_at_end lexer = lexer.current = String.length lexer.source
and skip lexer = lexer.current <- lexer.current + 1
