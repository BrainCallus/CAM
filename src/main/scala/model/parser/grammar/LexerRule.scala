package model.parser.grammar

final case class LexerRule[T <: Token](token: T, skip: Boolean)
