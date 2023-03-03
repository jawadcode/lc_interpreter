open Batteries
open Lexer

let () =
  let rec f () =
    let test = read_line () in
    let tokens = enum_of_string test in
    Printf.printf "Source: \"%s\"\nTokens:\n" test;
    Enum.map Token.to_string tokens |> Enum.iter print_endline;
    f ()
  in f ()