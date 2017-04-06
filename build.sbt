scalacOptions := Seq("-feature", "-deprecation")

scalaVersion := "2.12.1"

libraryDependencies += "net.bzzt" %% "scala-icalendar" % "0.0.1-SNAPSHOT"
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
