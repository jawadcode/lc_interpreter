open Batteries
open Parser

let rec main () =
  (try read_line () with End_of_file -> exit 1)
  |> Parser.run Parser.expr_parser
  |> Result.map (print_endline % Expr.expr_to_string)
  |> Result.iter_error (print_any stderr % Parser.error_to_string);
  main ()

let () =
  Printexc.record_backtrace true;
  main ()
