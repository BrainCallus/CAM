package cmd

import scopt.{OParser, OParserBuilder}

import java.nio.file.Path

object ScoptParser {
  val builder: OParserBuilder[SetupConfig] = OParser.builder[SetupConfig]
  val parser: OParser[Unit, SetupConfig] = {
    import builder._
    OParser.sequence(
      head("cam", "0.1.0"),
      opt[String]('i', "input")
        .required()
        .action((x, c) => c.copy(inputFileName = x))
        .text("Input file name"),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("Enable verbose output"),
      opt[Path]('d', "input-dir")
        .action((x, c) => c.copy(inputDir = x))
        .text("Directory for input files (default: $CAM_ROOT/storage)")
        .validate(p => if (p.toFile.isDirectory) success else failure(s"Can not find directory at specified path: $p")),
      checkConfig(cfg =>
        if (cfg.inputFile.toFile.isFile) success
        else failure(s"Can not find input file at specified path: ${cfg.inputFile}"),
      ),
    )
  }
}
