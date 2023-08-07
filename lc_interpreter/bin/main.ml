open Batteries
open Lexer
open Expr

let () =
  Printexc.record_backtrace true;
  let rec go () =
    let test = try read_line () with End_of_file -> exit 1 in
    let tokens = string_to_tokens test in
    Printf.printf "Source: %s\nTokens:\n" (dump test);
    Enum.map Token.to_string tokens |> Enum.iter print_endline;
    go ()
  in
  go ()
