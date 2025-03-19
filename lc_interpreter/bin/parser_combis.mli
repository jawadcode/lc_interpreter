open Batteries
open Utilities

type 'tok input = 'tok Span.spanned BatEnum.t

type 'tok error =
  | UnexpectedToken of { got : 'tok Span.spanned; expected : 'tok BatSet.t }
  | UnexpectedEndOfInput

val error_to_string : ('tok -> string) -> 'tok error -> string

type ('res, 'tok) t

val return : 'a -> ('tok, 'a) t
val fail : 'tok error -> ('tok, 'a) t
val just : 'tok -> ('tok, 'tok Span.spanned) t
val map : ('a -> 'b) -> ('tok, 'a) t -> ('tok, 'b) t
val bind : ('a -> ('tok, 'b) t) -> ('tok, 'a) t -> ('tok, 'b) t
val ( *> ) : ('tok, 'a) t -> ('tok, 'b) t -> ('tok, 'b) t
val ( <* ) : ('tok, 'a) t -> ('tok, 'b) t -> ('tok, 'a) t
val ( <*> ) : ('tok, 'a) t -> ('tok, 'b) t -> ('tok, 'a * 'b) t
val ( <|> ) : ('tok, 'a) t -> ('tok, 'a) t -> ('tok, 'a) t
val optional : ('tok, 'a) t -> ('tok, 'a option) t
val many : ('tok, 'a) t -> ('tok, 'a list) t
val run : ('a -> 'tok input) -> ('tok, 'b) t -> 'a -> ('b, 'tok error) result
