module Token : sig
  type t = { span : Utilities.Span.t; kind : token_kind }

  and token_kind

  val to_string : t -> string
end

type lexer

val enum_of_string : string -> Token.t BatEnum.t