open Utilities

module Token : sig
  type t = token_kind Span.spanned

  and token_kind =
    | TKLetKw
    | TKInKw
    | TKFunKw
    | TKIntLit
    | TKIdent
    | TKFatArrow
    | TKBind
    | TKLParen
    | TKRParen
    | TKAdd
    | TKSub
    | TKMul
    | TKDiv
    | TKMod
    | TKEof
    | TKError

  val to_string : t -> string
end

val string_to_tokens : string -> Token.t BatEnum.t
