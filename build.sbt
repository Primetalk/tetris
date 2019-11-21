
name := "Tetris"
version := "0.1-SNAPSHOT"
scalaVersion := "2.12.8"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  "org.querki" %%% "jquery-facade" % "1.2",
  "org.specs2" %%% "specs2-core" % "4.8.1" % Test,
  "org.specs2" %%% "specs2-scalacheck" % "4.8.1" % Test
)

jsDependencies += "org.webjars" % "jquery" % "2.2.1" / "jquery.js" minified "jquery.min.js"

scalaJSUseMainModuleInitializer := true

