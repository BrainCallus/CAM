import cats.effect.IO
import cats.syntax.all.*
import cmd.ScoptParser.parser
import cmd.SetupConfig
import model.CAMExecutor
import model.io.ContextWriter.ContextWriterConfig
import model.io.*
import model.parser.ml.{MlLexer, MlParser}
import scopt.{OEffect, OParser}
import tofu.syntax.feither.*
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global

import java.io.ByteArrayInputStream

object Entrypoint {
  def main(args: Array[String]): Unit = {

    (OParser.runParser(parser, args, SetupConfig()) match {
      case (Some(config), _) => runCam(config)
      case (None, effects) =>
        val usage = List(OParser.usage(parser))
        effects
          .collectFirst {
            case OEffect.DisplayToOut(msg: String) => msg
            case OEffect.DisplayToErr(msg: String) => msg
            case OEffect.ReportError(msg: String)  => msg
          }
          .map(_ :: usage)
          .getOrElse(usage)
          .map(Console[IO].errorln)
          .sequence_
    }).unsafeRunSync()
  }

  private def runCam(config: SetupConfig): IO[Unit] = {
    val reader = FileReader.make[IO]
    val camContextConfig: ContextWriterConfig = ContextWriterConfig(
      file = config.outputFile,
      termCol = 50,
      codeCol = 50,
      stackCol = 100,
    )

    implicit val rowFormater: RowFormater[IO]     = RowFormater.make[IO]
    implicit val fileWriter: FileWriter[IO]       = FileWriter.make[IO]
    implicit val contextWriter: ContextWriter[IO] = model.io.ContextWriter.make[IO](camContextConfig)

    val executor: CAMExecutor[IO] = CAMExecutor.make[IO]
    for {
      input <- reader.readInline(config.inputFile)
      is       = new ByteArrayInputStream(input.getBytes())
      mlParser = MlParser[IO](is)
      expr <- mlParser
        .parse()
        .run(mlParser.lex)
        .value
        .foldF(_.raiseError[IO, (MlLexer, MlParser.ParseContext)], _.pure[IO])
        .map(_._2.res)
      v <- executor.executeExpression(expr, config.verbose)
      _ <- Console[IO].println(s"Computation finished with result ${v.show}")
      _ <- Console[IO].println(s"See full output in ${config.outputFile}")
    } yield ()
  }
}
