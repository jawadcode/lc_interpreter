open Batteries
open Sedlexing

type token = { span : source_span; kind : token_kind }
and source_span = { start : Lexing.position; finish : Lexing.position }

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

let rec string_of_token { span; kind } =
  string_of_token_kind kind ^ string_of_source_span span

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

and string_of_source_span { start; finish } =
  " @ "
  ^ string_of_int start.pos_lnum
  ^ ":"
  ^ string_of_int start.pos_cnum
  ^ "..."
  ^ string_of_int finish.pos_lnum
  ^ ":"
  ^ string_of_int finish.pos_cnum

type token_seq = token Seq.t

let rec next_token (eof, lexbuf) =
  let digit = [%sedlex.regexp? '0' .. '9'] in
  let number = [%sedlex.regexp? Plus '0' .. '9'] in
  let alpha = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z'] in
  let alphanum = [%sedlex.regexp? alpha | digit] in
  let some_token kind =
    let span = lexing_positions lexbuf in
    Some (new_token span kind, (eof, lexbuf))
  in
  match%sedlex lexbuf with
  | white_space -> next_token (eof, lexbuf)
  | "let" -> some_token TKLetKw
  | "in" -> some_token TKInKw
  | "fun" -> some_token TKFunKw
  | number -> some_token TKIntLit
  | alpha, Star (alphanum | '_') -> some_token TKIdent
  | "=>" -> some_token TKFatArrow
  | '=' -> some_token TKEquals
  | '(' -> some_token TKLParen
  | ')' -> some_token TKRParen
  | '+' -> some_token TKAdd
  | '-' -> some_token TKSub
  | '*' -> some_token TKMul
  | '/' -> some_token TKDiv
  | eof ->
      if eof then None
      else
        let span = lexing_positions lexbuf in
        Some (new_token span TKEof, (true, lexbuf))
  | _ ->
      let tok = new_token (lexing_positions lexbuf) TKError in
      Some (tok, (eof, lexbuf))

and new_token (start, finish) kind = { span = { start; finish }; kind }

let lex source =
  let lexbuf = Utf8.from_string source in
  Seq.unfold next_token (false, lexbuf)
