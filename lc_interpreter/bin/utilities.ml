open Lexing
open Batteries

module Span = struct
  type t = { start : int; finish : int }
  and 'a spanned = { span : t; source : Substring.t; node : 'a }

  let ( + ) lhs rhs = { start = lhs.start; finish = rhs.finish }
  let map f spanned = { spanned with node = f spanned.node }
  let map_source f spanned = { spanned with node = f spanned.source }

  let from_positions start finish =
    { start = start.pos_bol; finish = finish.pos_bol }

  let to_string { start; finish } =
    string_of_int start ^ ".." ^ string_of_int finish

  let fmt f { span; source = _; node } =
    Printf.sprintf "%s @ %s" (f node) (to_string span)
end

module Ops = struct
  let ( let+ ) x f = Option.map f x
  and ( let* ) = Option.bind
end
