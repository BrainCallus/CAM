package model.io

import cats.Monad
import cats.nio.file.Files
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, StandardOpenOption}

trait FileWriter[F[_]] {
  def write(path: Path, content: String): F[Unit]
  def writeToExisting(path: Path, content: String): F[Unit]
}

object FileWriter {
  def apply[F[_]](implicit fw: FileWriter[F]): FileWriter[F] = fw

  def make[F[_]: Monad: Files]: FileWriter[F] = new FileWriter[F] {
    override def write(path: Path, content: String): F[Unit] =
      internalWrite(path, content, StandardOpenOption.TRUNCATE_EXISTING)

    override def writeToExisting(path: Path, content: String): F[Unit] =
      internalWrite(path, content, StandardOpenOption.APPEND)

    private def internalWrite(path: Path, content: String, option: StandardOpenOption): F[Unit] = {
      val absPath = path.toAbsolutePath
      for {
        _ <- createNonExisting(absPath)
        _ <- Files[F].write(absPath, content.getBytes(StandardCharsets.UTF_8), option)
      } yield ()
    }

    private def createNonExisting(path: Path): F[Boolean] =
      for {
        needToCreate <- Files[F].exists(path).map(!_)
        _ <- Monad[F].whenA(needToCreate) {
          Files[F].createDirectories(path.getParent) *> Files[F].createFile(path)
        }
      } yield needToCreate
  }
}
