ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.beepboop"

scalaVersion := "3.6.4"

name := "minizinc-model-generator"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"
libraryDependencies += "org.yaml" % "snakeyaml" % "2.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "com.lihaoyi" %% "mainargs" % "0.7.6"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.8"
libraryDependencies ++= Seq(
  "com.github.pureconfig" %% "pureconfig-core"      % "0.17.9",
  "com.github.pureconfig" %% "pureconfig-yaml"      % "0.17.9"
)

javacOptions ++= Seq("-source", "21", "-target", "21")

enablePlugins(Antlr4Plugin)
enablePlugins(JavaAppPackaging)

cancelable in Global := true

Antlr4 / antlr4PackageName := Some("com.beepboop.parser")
Antlr4 / antlr4Version := "4.13.1"
Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4GenListener := true

libraryDependencies ++= Seq(
  "org.antlr" % "antlr4-runtime" % "4.13.2",
)

fork in Global := true

enablePlugins(Antlr4Plugin)

//Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/java"