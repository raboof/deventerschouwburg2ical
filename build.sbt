scalacOptions := Seq("-feature", "-deprecation")

scalaVersion := "2.11.8"

scalafmtConfig in ThisBuild := Some(file(".scalafmt"))

libraryDependencies += "default" %% "scala-icalendar" % "0.1-SNAPSHOT"
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "1.1.0-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
