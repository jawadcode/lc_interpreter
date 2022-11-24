open Sedlexing

type token_kind =
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

type source_span = { start : Lexing.position; finish : Lexing.position }
type token = { span : source_span; kind : token_kind }

let new_token (start, finish) kind = { span = { start; finish }; kind }

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
  match%sedlex lexbuf with
  | white_space -> next_token (eof, lexbuf)
  | "let" -> some_token TKLetKw eof lexbuf
  | "in" -> some_token TKInKw eof lexbuf
  | "fun" -> some_token TKFunKw eof lexbuf
  | number -> some_token TKIntLit eof lexbuf
  | alpha, Star (alphanum | '_') -> some_token TKIdent eof lexbuf
  | "=>" -> some_token TKFatArrow eof lexbuf
  | '=' -> some_token TKEquals eof lexbuf
  | '(' -> some_token TKLParen eof lexbuf
  | ')' -> some_token TKRParen eof lexbuf
  | '+' -> some_token TKAdd eof lexbuf
  | '-' -> some_token TKSub eof lexbuf
  | '*' -> some_token TKMul eof lexbuf
  | '/' -> some_token TKDiv eof lexbuf
  | eof -> if eof then None else some_token TKEof true lexbuf
  | _ ->
      let tok = new_token (lexing_positions lexbuf) TKError in
      Some (tok, (eof, lexbuf))

and some_token kind eof lexbuf =
  let start, finish = lexing_positions lexbuf in
  let tok = { span = { start; finish }; kind } in
  Some (tok, (eof, lexbuf))

let lex source =
  let lexbuf = Utf8.from_string source in
  Seq.unfold next_token (false, lexbuf)
