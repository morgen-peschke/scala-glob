import sbt._
import Keys._

object BuildScalaGlob extends Build {
  val commonDependencies = Seq(
    "com.lihaoyi" %% "fastparse" % "0.4.1",
    "org.scalatest"  %% "scalatest" % "2.2.2" % "test",
    "org.scalamock"  %% "scalamock-scalatest-support" % "3.2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0")

  val commonSettings = Seq(
    version := "0.0",
    scalaVersion := "2.11.7",
    libraryDependencies ++= commonDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"))

  lazy val scalaGlob = project
    .in(file("."))
    .settings(commonSettings)
}
