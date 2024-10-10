val zioPreludeVersion = "1.0.0-RC15"

name := "scala-problems-interviews"

version := "0.1"

scalaVersion := "3.5.0"

libraryDependencies ++= Seq(
  "io.github.hughsimpson" %% "scalameter" % "0.22.1",
  "dev.zio" %% "zio-prelude" % zioPreludeVersion,
  "dev.zio" %% "zio-json" % "0.3.0"
)

fork := true

outputStrategy := Some(StdoutOutput)

connectInput := true

scalacOptions ++= Seq("-Yexplicit-nulls")
