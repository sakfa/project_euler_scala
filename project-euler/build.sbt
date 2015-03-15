name := "project-euler"

version := "1.0"

scalaVersion := "2.10.4"

mainClass in (Compile, run) := Some("pl.sakfa.project_euler.runner.SolutionsRunner")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"

libraryDependencies ++= Seq(
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
    "org.slf4j" % "slf4j-log4j12" % "1.7.5",
    "log4j" % "log4j" % "1.2.17",
    "com.github.nscala-time" %% "nscala-time" % "1.8.0"
)