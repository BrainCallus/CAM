package model.parser.template

import model.parser.grammar.Token

final case class InterimToken(name: String, pattern: String) extends Token {
  override def getName: String = name
}
