open Batteries
open Utilities

type 'tok input = 'tok Span.spanned Enum.t

type 'tok error =
  | UnexpectedToken of { got : 'tok Span.spanned; expected : 'tok Set.t }
  | UnexpectedEndOfInput

let error_to_string tok_to_string =
  let fmt_expected = Set.map tok_to_string %> Set.to_list %> String.join ", " in
  function
  | UnexpectedToken { got = { span; source; node = _ }; expected } ->
      Printf.sprintf "Syntax Error [%d-%d]: Expected %s, got '%s'" span.start
        span.finish (fmt_expected expected)
        (Substring.to_string source)
  | UnexpectedEndOfInput -> "Syntax Error: Unexpected end of input"

type ('tok, 'res) t = {
  run : 'tok input -> 'tok input * ('res, 'tok error) result;
}

let return x = { run = (fun input -> (input, Ok x)) }
let fail err = { run = (fun input -> (input, Error err)) }

let just tk =
  {
    run =
      (fun input ->
        ( input,
          match Enum.get input with
          | Some tk2 when tk2.node == tk -> Ok tk2
          | Some tk2 ->
              Error
                (UnexpectedToken { got = tk2; expected = BatSet.singleton tk })
          | None -> Error UnexpectedEndOfInput ));
  }

let map f parser =
  {
    run =
      ( function
      | input, Ok x -> (input, Ok (f x))
      | input, Error err -> (input, Error err) )
      % parser.run;
  }

let bind f parser =
  {
    run =
      ( function
      | input, Ok x -> (f x).run input
      | input, Error err -> (input, Error err) )
      % parser.run;
  }

let ( *> ) parser1 parser2 =
  {
    run =
      (fun input ->
        let input, res = parser1.run input in
        match res with
        | Ok _ -> parser2.run input
        | Error err -> (input, Error err));
  }

let ( <* ) parser1 parser2 =
  {
    run =
      (fun input ->
        let input, res = parser1.run input in
        match res with
        | Ok x -> (
            let input, res = parser2.run input in
            match res with
            | Ok _ -> (input, Ok x)
            | Error err -> (input, Error err))
        | Error err -> (input, Error err));
  }

let ( <*> ) parser1 parser2 =
  {
    run =
      (fun input ->
        let input, res = parser1.run input in
        match res with
        | Ok x -> (
            let input, res = parser2.run input in
            match res with
            | Ok y -> (input, Ok (x, y))
            | Error err -> (input, Error err))
        | Error err -> (input, Error err));
  }

(* TODO: Build up a set of expected tokens *)
let ( <|> ) parser1 parser2 =
  {
    run =
      (fun input ->
        let input, res = parser1.run input in
        match res with Ok x -> (input, Ok x) | Error _ -> parser2.run input);
  }

let optional parser =
  {
    run =
      (fun input ->
        let input, res = parser.run input in
        match res with
        | Ok x -> (input, Ok (Some x))
        | Error _ -> (input, Ok None));
  }

let many parser =
  {
    run =
      (fun input ->
        let xs = ref [] in
        let rec loop input =
          let input, res = parser.run input in
          match res with
          | Ok x ->
              xs := x :: !xs;
              loop input
          | Error _ -> input
        in
        let input = loop input in
        (input, Ok (List.rev !xs)));
  }

let run lex parser = lex %> parser.run %> Tuple2.second
