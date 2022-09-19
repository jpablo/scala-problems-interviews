name := "scala-problems-interviews"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "io.github.hughsimpson" %% "scalameter" % "0.22.1"
)

fork := true

outputStrategy := Some(StdoutOutput)

connectInput := true