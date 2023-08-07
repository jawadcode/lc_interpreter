open Lexing
open Batteries

module Span = struct
  type t = { start : int; finish : int }
  and 'a spanned = { span : t; node : 'a }

  let ( + ) lhs rhs = { start = lhs.start; finish = rhs.finish }
  let map_span f s = { s with node = f s.node }

  let from_positions start finish =
    { start = start.pos_bol; finish = finish.pos_bol }

  let sub string { start; finish } = String.sub string start (finish - start)

  let to_string { start; finish } =
    string_of_int start ^ ".." ^ string_of_int finish
end

module Ops = struct
  let ( let+ ) x f = Option.map f x
  and ( let* ) = Option.bind
end
