name := "scala-problems-interviews"

version := "0.1"

scalaVersion := "3.2.0"

libraryDependencies ++= Seq(
  "io.github.hughsimpson" %% "scalameter" % "0.22.1",
  "dev.zio" %% "zio-json" % "0.3.0"
)

fork := true

outputStrategy := Some(StdoutOutput)

connectInput := true

scalacOptions ++= Seq("-Yexplicit-nulls")