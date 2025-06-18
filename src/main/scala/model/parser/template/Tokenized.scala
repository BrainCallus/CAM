package model.parser.template

import model.parser.grammar.Token

sealed trait Tokenized {
  def text: String
}

object Tokenized {
  case object TokenizedEmpty extends Tokenized {
    override def text: String = ""
  }

  final case class TokenizedValue[+T <: Token](token: T, text: String, startIdx: Int, skip: Boolean) extends Tokenized {
    def endIdx: Int = startIdx + text.length

    def tokenLen: Int = text.length
  }
}
