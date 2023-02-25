open Batteries
open Lexer

let () =
  let rec f () =
    let test = read_line () in
    let tokens = lex test in
    Printf.printf "Source: \"%s\"\nTokens:\n" test;
    Seq.map string_of_token tokens |> Seq.iter print_endline;
    f ()
  in f ()