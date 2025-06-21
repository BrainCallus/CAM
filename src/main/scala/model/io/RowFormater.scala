package model.io

import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{ApplicativeError, Monad, MonadError, MonadThrow}

import java.io.IOException
import scala.annotation.tailrec

trait RowFormater[F[_]] {
  def formatRaw(row: List[(String, Int)], sep: Option[String] = None): F[String]
  def formatMd(row: List[String], sep: Option[String] = None): F[String]
}

object RowFormater {
  def apply[F[_]](implicit fmt: RowFormater[F]): RowFormater[F] = fmt

  def make[F[_]: MonadThrow]: RowFormater[F] = new RowFormater[F] {
    override def formatRaw(row: List[(String, Int)], sep: Option[String]): F[String] = {
      for {
        _ <- MonadThrow[F].whenA(row.exists { case (_, i) =>
          i < 1
        })(new IOException("All columns should have length at least 1").raiseError)
        entries = row.map { case (value, limit) =>
          formatLine(value, limit)()
        }
      } yield getFormattedLines(entries.zip(row.map(_._2 + 2)), sep)().mkString("\n")
    }

    override def formatMd(row: List[String], sep: Option[String]): F[String] =
      row.mkString(sep = sep.getOrElse(" | "), start = "| ", end = " |").pure[F]

    @tailrec
    private def getFormattedLines(entries: List[(List[String], Int)], sep: Option[String])(
      accum: List[String] = List.empty,
    ): List[String] = {
      if (entries.exists(_._1.nonEmpty)) {
        val lines = entries.map {
          case e @ (Nil, l)     => ("".padTo(l, ' '), e)
          case (x :: xs, limit) => (x.padTo(limit, ' '), (xs, limit))
        }
        getFormattedLines(lines.map(_._2), sep)(
          lines.map(_._1).mkString(sep = sep.getOrElse(" | "), start = "| ", end = " |") :: accum,
        )
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
}
