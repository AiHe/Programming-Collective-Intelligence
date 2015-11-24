// name := "Programming-Collective-Intelligence"
//
// version := "1.0"
//
// scalaVersion := "2.11.7"
//
// libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.3"

name := "Programming-Collective-Intelligence"

val commonSettings = Seq(
  organization := "me.heai",
  version := "1.0",
  scalaVersion := "2.11.7",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

lazy val common = project.in(file("common"))
  .settings(commonSettings:_*)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.0-M3")

lazy val chapter8 = project.in(file("chapter8"))
  .settings(commonSettings:_*)
  .dependsOn(common)

lazy val chapter9 = project.in(file("chapter9"))
  .settings(commonSettings:_*)
  .dependsOn(common)

lazy val chapter7 = project.in(file("chapter7"))
  .settings(commonSettings:_*)
  .dependsOn(common)

lazy val chapter6= project.in(file("chapter6"))
  .settings(commonSettings:_*)
  .dependsOn(common)

lazy val chapter5 = project.in(file("chapter5"))
  .settings(commonSettings:_*)
  .dependsOn(common)

lazy val main = project.in(file("."))
  .aggregate(common, chapter5, chapter6, chapter7, chapter8, chapter9)
