module Span : sig
  type t = { start : int; finish : int }

  val from_positions : Lexing.position -> Lexing.position -> t

  val sub : string -> t -> string

  val to_string : t -> string
end