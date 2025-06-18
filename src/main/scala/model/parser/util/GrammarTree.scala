package model.parser.util

import cats.Show
import model.parser.template.Tokenized

sealed abstract class GrammarTree[T](val root: T, val children: List[GrammarTree[_]]) extends Tree[T] {}

object GrammarTree {
  class ContextTree(root: String, children: List[GrammarTree[_]]) extends GrammarTree[String](root, children) {
    override implicit def showEv: Show[String] = showString

    def pushFirstChild(child: GrammarTree[_]) = new ContextTree(root, child :: children)
    def appendLastChild(child: GrammarTree[_]) = new ContextTree(root, children ++ List(child))
  }

  case class TerminalTree(tokenized: Tokenized) extends GrammarTree[Tokenized](tokenized, List.empty) {
    override implicit def showEv: Show[Tokenized] = showTokenized
  }

  implicit val showString: Show[String] = (str: String) => str
  implicit val showTokenized: Show[Tokenized] = (tokenized: Tokenized) => tokenized.text
}
