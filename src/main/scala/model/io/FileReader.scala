package model.io

import cats.{Monad, MonadThrow}
import cats.nio.file.Files
import cats.syntax.all.*

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.util.stream.Stream
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait FileReader[F[_]] {
  def readInline(path: Path): F[String]
}

object FileReader {
  def apply[F[_]](implicit reader: FileReader[F]): FileReader[F] = reader

  def make[F[_]: Monad: Files]: FileReader[F] = new FileReader[F] {
    override def readInline(path: Path): F[String] = {
      Files[F].lines(path, StandardCharsets.UTF_8).map(fromStream[String].andThen(_.mkString(" ")))
    }

    private def fromStream[T](stream: Stream[T]): List[T] = {
      stream.toList.asScala.toList
    }
  }
}
