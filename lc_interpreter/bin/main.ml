open Batteries
open Parser.Expr
open Utilities

let rec main () =
  (* try read_line () with End_of_file -> exit 1)
     |> Parser.run Parser.expr_parser
     |> Result.map (print_endline % Expr.expr_to_string)
     |> Result.iter_error (print_any stderr % Parser.error_to_string);
     main () *)
  (* Source String: "let id = fun x => x in id 123" *)
  let test =
    {
      Span.span = { start = 0; finish = 29 };
      node =
        ELet
          {
            name = { Span.span = { start = 4; finish = 6 }; node = "id" };
            value =
              {
                Span.span = { start = 9; finish = 19 };
                node =
                  EFun
                    {
                      arg =
                        { Span.span = { start = 13; finish = 14 }; node = "x" };
                      body =
                        {
                          Span.span = { start = 18; finish = 19 };
                          node =
                            EIdent
                              {
                                Span.span = { start = 18; finish = 19 };
                                node = "x";
                              };
                        };
                    };
              };
            body =
              {
                Span.span = { start = 23; finish = 29 };
                node =
                  EApp
                    {
                      func =
                        {
                          Span.span = { start = 23; finish = 24 };
                          node =
                            EIdent
                              {
                                Span.span = { start = 23; finish = 24 };
                                node = "id";
                              };
                        };
                      arg =
                        {
                          Span.span = { start = 26; finish = 29 };
                          node = EIntLit 123;
                        };
                    };
              };
          };
    }
  in
  print_string (expr_to_string test)

let () =
  Printexc.record_backtrace true;
  main ()
