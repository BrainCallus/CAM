package cmd

import cmd.SetupConfig.STORAGE_PATH

import java.nio.file.{Path, Paths}

final case class SetupConfig(
  inputFileName: String = "",
  inputDir: Path = STORAGE_PATH,
  verbose: Boolean = false,
) {
  def inputFile: Path = inputDir.resolve(inputFileName)

  def outputFileName: String = {
    val extIdx         = inputFileName.lastIndexOf('.')
    val nameWithoutExt = inputFileName.substring(0, if (extIdx > 0) extIdx else inputFileName.length)
    s"${nameWithoutExt}_result.md"
  }

  def outputFile: Path = inputDir.resolve(outputFileName)
}

object SetupConfig {
  private val CAM_ROOT   = sys.env.getOrElse("CAM_ROOT", ".")
  val STORAGE_PATH: Path = Paths.get(CAM_ROOT).resolve("storage")
}
