name := """Anorm Query Helper"""

organization := "ru.dimonz80"

version := "0.1"

scalaVersion := "2.13.1"

val anormVersion = "2.6.5"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.playframework.anorm" %% "anorm" % anormVersion

libraryDependencies += "com.h2database" % "h2" % "1.4.200" % Test

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test

// all in UTF-8!!!
javacOptions ++= Seq("-encoding", "UTF-8")