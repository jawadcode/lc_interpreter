module Span : sig
  type t = { start : int; finish : int }
  and 'a spanned = { span : t; source : BatSubstring.t; node : 'a }

  val ( + ) : t -> t -> t
  val map : ('a -> 'b) -> 'a spanned -> 'b spanned
  val map_source : (BatSubstring.t -> 'b) -> 'a spanned -> 'b spanned
  val from_positions : Lexing.position -> Lexing.position -> t
  val to_string : t -> string
  val fmt : ('a -> string) -> 'a spanned -> string
end

module Ops : sig
  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
end
