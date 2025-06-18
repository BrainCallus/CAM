package model.parser.grammar.entry

import model.parser.util.Ternary.Ternary

import scala.annotation.tailrec

case class TranslatingSymbol(code: String) extends GrammarEntry {
  def normalize(lexerVar: String): TranslatingSymbol = {
    TranslatingSymbol(internalNormalize(new StringBuilder(), code.length, in = false)(code, lexerVar))
  }

  @tailrec
  private def internalNormalize(stringBuilder: StringBuilder, n: Int, in: Boolean)(
    code: String,
    lexerVar: String
  ): String =
    n match {
      case 0 => stringBuilder.toString()
      case x =>
        if (code.charAt(code.length - x) == '$' && !in) {
          internalNormalize(stringBuilder.append(""), n - 1, in)(code, lexerVar)
        } else {
          val curSymbol = code.charAt(code.length - x)
          internalNormalize(stringBuilder.append(curSymbol), n - 1, maybeInvert(curSymbol)(in))(code, lexerVar)
        }
    }

  private def maybeInvert(curSymbol: Char): Boolean => Boolean =
    (curSymbol == '"') ?? (x => !x, identity)
}
