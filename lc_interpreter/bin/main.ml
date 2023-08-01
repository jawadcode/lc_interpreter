open Batteries
open Lexer

let () =
  let rec go () =
    let test = read_line () in
    let tokens = string_to_tokens test in
    Printf.printf "Source: \"%s\"\nTokens:\n" test;
    Enum.map Token.to_string tokens |> Enum.iter print_endline;
    go ()
  in
  go ()
