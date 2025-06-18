package model.parser.util

import cats.data.State
import model.parser.template.Tokenized.TokenizedValue
import CommonUtils.{convertList, foldl}
import GrammarTree.TerminalTree

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path}
import scala.sys.process.Process
import scala.util.Try

object WriteUtil {

  def clearDirectoryFiles(dir: Path): Try[Boolean] = {
    val files = convertList(Files.walk(dir).filter(f => Files.isRegularFile(f)).toList)
    foldl(Try(true))((s: Try[Boolean], file: Path) => s.flatMap(_ => deleteFile(file).map(_ => true)))(files)
  }

  def deleteFile(file: Path): Try[Unit] =
    Try(Files.delete(file))

  def makeGraph(parsed: Tree[_], file: String = "graph.dot"): Unit = {
    try {
      val fileWriter = new FileWriter(new File(file))
      fileWriter.write(toDotFileString(parsed))
      fileWriter.close()
      Process(s"dot -v -Tpng -O $file").run()
      println("Graph successfully created.")
    } catch {
      case e: Exception => println(s"${e.getMessage}\n${e.getStackTrace.mkString("StackTrace:\n  ", "\n  ", "")}")
    }
  }

  private def toDotFileString(tree: Tree[_]): String =
    internalWriteTreeDot(new StringBuilder("digraph G {\n"), tree)(0, -1)._2.append("}\n").toString()

  private def internalWriteTreeDot(
    stringBuilder: StringBuilder,
    tree: Tree[_]
  )(idx: Int, pIdx: Int): (Int, StringBuilder) = {

    val nextBuilder: StringBuilder = stringBuilder
      .append(s"$idx [label = \"${getDotLabelFromTree(tree)}\"]\n")
      .append(if (pIdx == -1) "" else s"$pIdx -> $idx\n")
    tree.children.foldLeft((idx, nextBuilder))((acc: (Int, StringBuilder), child: Tree[_]) => {
      internalWriteTreeDot(acc._2, child)(acc._1 + 1, idx)
    })
  }

  private def getDotLabelFromTree(tree: Tree[_]): String = {
    tree match {
      case TerminalTree(tokenized) => s"[${tokenized.asInstanceOf[TokenizedValue[_]].token}, ${tokenized.text}]"
      case _                       => tree.showRoot
    }
  }

  def writeState(writer: BufferedWriter, line: String)(modify: Int => Int): State[Int, Unit] =
    State(offset =>
      (
        modify(offset),
        writeLine(writer, line, offset)
      )
    )

  def writeLine(writer: BufferedWriter, line: String, offsetLen: Int): Unit = {
    writeOffset(writer, offsetLen)
    writer.write(line)
    writer.newLine()
  }

  private def writeOffset(writer: BufferedWriter, level: Int): Unit =
    writer.write(SCALA_TAB.repeat(level))

  private val SCALA_TAB = "  "
}
