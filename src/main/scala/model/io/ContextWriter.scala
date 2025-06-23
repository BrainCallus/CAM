package model.io
import cats.effect.std.Console
import cats.{Monad, Show}
import model.{CAMContext, Instruction, Value}

import java.nio.file.Path
import cats.syntax.all.*
import model.Value.PairValue

trait ContextWriter[F[_]] {
  def consolePrint(ctx: CAMContext): F[Unit]
  def writeTableHeader(): F[Unit]
  def writeAsTableEntry(ctx: CAMContext): F[Unit]
}

object ContextWriter {
  def apply[F[_]](implicit writer: ContextWriter[F]): ContextWriter[F] = writer

  def make[F[_]: Monad: FileWriter: RowFormater: Console](
    config: ContextWriterConfig,
  ): ContextWriter[F] = Impl(config)

  final case class ContextWriterConfig(file: Path, termCol: Int, codeCol: Int, stackCol: Int) {
    def totalLength: Int = termCol + codeCol + stackCol
  }

  given showArray[T: Show]: Show[Iterable[T]] = { arr =>
    arr.map(_.show).mkString("[", ", ", "]")
  }

  given showCode: Show[Iterable[Instruction]] = { code =>
    Show[Iterable[Instruction]](using showArray[Instruction]).show(code)
  }

  given showIterableStrings: Show[Iterable[String]] = { values =>
    Show[Iterable[String]](using showArray[String]).show(values)
  }

  private case class Impl[F[_]: Monad: FileWriter: RowFormater: Console](config: ContextWriterConfig)
      extends ContextWriter[F] {
    override def consolePrint(ctx: CAMContext): F[Unit] = {
      for {
        s <- RowFormater[F].formatRaw(
          List(
            showValue(ctx.term, ctx.env)                                   -> config.termCol,
            showCode.show(ctx.code)                                        -> config.codeCol,
            showIterableStrings.show(ctx.stack.map(showValue(_, ctx.env))) -> config.stackCol,
          ),
        )
        _ <- Console[F].println(s)
        _ <- Console[F].println("-".repeat(config.totalLength + 15))
      } yield ()
    }

    override def writeTableHeader(): F[Unit] = {
      for {
        heads <- FileWriter[F].write(
          config.file,
          List(
            " Term ",
            " Code ",
            " Stack ",
          ).mkString("|", "|", "|\n"),
        )
        _ <- FileWriter[F].writeToExisting(config.file, "|:-:|:-:|:-:|\n")
      } yield ()
    }

    override def writeAsTableEntry(ctx: CAMContext): F[Unit] = {
      for {
        s <- RowFormater[F].formatMd(
          List(
            showValue(ctx.term, ctx.env),
            showCode.show(ctx.code),
            showIterableStrings.show(ctx.stack.map(showValue(_, ctx.env))),
          ),
        )
        _ <- FileWriter[F].writeToExisting(config.file, s"$s\n")
      } yield ()
    }

    private def showValue(value: Value, env: List[Value]): String = {
      value match {
        case p: PairValue => p.showValues(env)
        case v            => v.show
      }
    }
  }
}
