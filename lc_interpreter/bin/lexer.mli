open Utilities

module Token : sig
  type t = { span : Span.t; kind : token_kind }

  and token_kind =
    | TKLetKw
    | TKInKw
    | TKFunKw
    | TKIntLit
    | TKIdent
    | TKFatArrow
    | TKEquals
    | TKLParen
    | TKRParen
    | TKAdd
    | TKSub
    | TKMul
    | TKDiv
    | TKEof
    | TKError

  val to_string : t -> string
end

val string_to_tokens : string -> Token.t BatEnum.t
