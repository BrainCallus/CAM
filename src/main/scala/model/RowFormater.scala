package model

import scala.annotation.tailrec

object RowFormater {
  def formatRow(row: List[(String, Int)], sep: Option[String] = None): String = {
    val safeIntRow = row.map { case (s, i) =>
      (s, Math.max(1, i))
    }
    val entries = safeIntRow.map { case (value, limit) =>
      formatLine(value, limit)()
    }
    getFormattedLines(entries.zip(safeIntRow.map(_._2 + 2)), sep)().mkString("\n")
  }

  @tailrec
  private def getFormattedLines(entries: List[(List[String], Int)], sep: Option[String])(
    accum: List[String] = List.empty,
  ): List[String] = {
    if (entries.exists(_._1.nonEmpty)) {
      val lines = entries.map {
        case e @ (Nil, l)     => ("".padTo(l, ' '), e)
        case (x :: xs, limit) => (x.padTo(limit, ' '), (xs, limit))
      }
      getFormattedLines(lines.map(_._2), sep)(lines.map(_._1).mkString(sep.getOrElse("")) :: accum)
    } else {
      accum.reverse
    }
  }

  @tailrec
  private def formatLine(line: String, limit: Int)(accum: List[String] = List.empty): List[String] =
    if (line.length <= limit) {
      (line :: accum).reverse
    } else {
      formatLine(line.substring(limit), limit)(line.substring(0, limit) :: accum)
    }
}
