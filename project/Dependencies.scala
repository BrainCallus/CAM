import Versions.V
import sbt.Cross
import sbt.librarymanagement.{CrossVersion, ModuleID}
import sbt.librarymanagement.syntax.*

object Dependencies {
  object cats {
    val core   = "org.typelevel"     %% "cats-core"       % V.catsCore
    val effect = "org.typelevel"     %% "cats-effect"     % V.catsEffect
    val std    = "org.typelevel"     %% "cats-effect-std" % V.catsEffect
    val nio    = "io.github.akiomik" %% "cats-nio-file"   % V.catsNioFile

    def * : Seq[ModuleID] = Seq(core, effect, std, nio)
  }

  object enumeratum {
    val base = "com.beachape" %% "enumeratum" % V.enumeratum

    def * : Seq[ModuleID] = Seq(base)
  }

  object tethys {
    val core       = "com.tethys-json" %% "tethys-core"       % V.tethys
    val jackson    = "com.tethys-json" %% "tethys-jackson213" % V.tethys
    val enumeratum = "com.tethys-json" %% "tethys-enumeratum" % V.tethys

    def * : Seq[ModuleID] = Seq(core, jackson, enumeratum)
  }

  object tofu {
    val core = "tf.tofu" %% "tofu-core-ce3" % V.tofu
    val full = "tf.tofu" %% "tofu"          % V.tofu
  }

  object scopt {
    val scopt = "com.github.scopt" %% "scopt" % "4.1.0"

    def * : Seq[ModuleID] = Seq(scopt)
  }
}
