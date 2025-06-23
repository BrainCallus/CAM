package model.parser.util

import GrammarTree.*

import scala.annotation.tailrec
import scala.jdk.javaapi.CollectionConverters.asScala

object CommonUtils {
  def convertList[T](list: java.util.List[T]): List[T] = {
    asScala(list.iterator()).toList
  }

  // for tests mainly
  def treeToStringList(tree: GrammarTree[?]): List[String] = internalTreeToStringList(List.empty[String])(tree)

  private def internalTreeToStringList(acc: List[String])(tree: GrammarTree[?]): List[String] =
    tree match {
      case TerminalTree(tokenized) => acc ++ List(tokenized.text)
      case _ =>
        foldl(acc)((accum: List[String], tree: GrammarTree[?]) => {
          accum ++ treeToStringList(tree)
        })(tree.children)
    }

  def foldl[T, E](accum: E)(func: (E, T) => E)(list: List[T]): E = doFoldl(accum)(func)(list)

  @tailrec
  private def doFoldl[T, E](accum: E)(
    func: (E, T) => E,
  )(list: List[T]): E =
    list match {
      case Nil          => accum
      case head :: tail => doFoldl(func(accum, head))(func)(tail)
    }

  def getDefaultValueByTypeAsString(typeName: String): String = {
    typeName match {
      case "Double" | "Int" | "Long" | "`Short`" | "Float" | "Byte" => "0"
      case "Char"                                                   => "'\u0000'"
      case "Boolean"                                                => "false"
      case "Unit"                                                   => "()"
      case _                                                        => "null"
    }
  }
}
