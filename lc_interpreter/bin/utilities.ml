open Lexing

module Span = struct
  type t = { start : int; finish : int }

  let from_positions start finish =
    { start = start.pos_bol; finish = finish.pos_bol }

  let sub string { start; finish } = String.sub string start (finish - start)

  let to_string { start; finish } =
    string_of_int start ^ ".." ^ string_of_int finish
end
