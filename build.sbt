name := """Anorm Query Helper"""

organization := "ru.dimonz80"

version := "0.1"

scalaVersion := "2.12.8"

lazy val anormVersion = "2.6.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.playframework.anorm" %% "anorm" % anormVersion

// all in UTF-8!!!

javacOptions ++= Seq( "-encoding", "UTF-8" )