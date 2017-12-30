name := """carpooling"""
organization := "com.tomaszwiech"

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"


libraryDependencies += "org.mongodb" %% "casbah" % "3.1.1"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % Test
libraryDependencies += "joda-time" % "joda-time" % "2.3"



// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.tomaszwiech.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.tomaszwiech.binders._"
