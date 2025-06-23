ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "CAM",
    libraryDependencies ++=
      Dependencies.cats.* ++
        Dependencies.tethys.* ++
        Seq(Dependencies.tofu.full.exclude("org.typelevel", "cats-effect_3")) ++
        Dependencies.enumeratum.* ++
        Dependencies.scopt.*,
    scalacOptions ++= Seq(
      "-language:experimental.macros",
      "-source:3.4-migration",
      "-rewrite",
      "-Wconf:src=src/main/scala/model/parser/ml/.*:s",
    ),
    ThisBuild / libraryDependencySchemes += "org.typelevel" %% "cats-effect" % VersionScheme.Always,
  )

lazy val runCam =
  inputKey[Unit]("Run CAM")

runCam :=
  (root / Compile / runMain).partialInput(" Entrypoint").evaluated
