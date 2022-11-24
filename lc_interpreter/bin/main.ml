open Lexer

let () =
  let test = "add 123 123" in
  let tokens = lex test in
  Printf.printf "Test string: \"%s\"\nTokens:\n" test;
  Seq.map string_of_token tokens
  |> Seq.iter print_endline