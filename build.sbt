ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "CAM",
    libraryDependencies ++=
      Dependencies.cats.* ++
        Dependencies.tethys.* ++
        Seq(Dependencies.tofu.full.exclude("org.typelevel", "cats-effect_3")) ++
        Dependencies.enumeratum.*,
    scalacOptions ++= Seq(
      "-language:experimental.macros",
    ),
    ThisBuild / libraryDependencySchemes += "org.typelevel" %% "cats-effect" % VersionScheme.Always,
  )
