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
        .text(
          "Input file name. The output file will be created in the same directory with a '_result' suffix and a\n\t" +
            "'.md' extension. If the file already exists, it will be overwritten.",
        ),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text(
          "Enable verbose output: all output will be duplicated to the console, in addition to being written to the\n\t" +
            "output file.",
        ),
      opt[Path]('d', "input-dir")
        .action((x, c) => c.copy(inputDir = x))
        .text("Specify directory for input file (default: $CAM_ROOT/storage)")
        .validate(p => if (p.toFile.isDirectory) success else failure(s"Can not find directory at specified path: $p")),
      checkConfig(cfg =>
        if (cfg.inputFile.toFile.isFile) success
        else failure(s"Can not find input file at specified path: ${cfg.inputFile}"),
      ),
    )
  }
}
