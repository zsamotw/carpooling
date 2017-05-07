name := """carpooling"""
organization := "com.tomaszwiech"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

//libraryDependencies += filters
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % Test
libraryDependencies +=  "com.typesafe.play" %% "play-slick" % "2.0.0"
libraryDependencies +="com.typesafe.play" %% "play-slick-evolutions" % "2.0.0"
libraryDependencies += "com.h2database" % "h2" % "1.4.194"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.8.6"




// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.tomaszwiech.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.tomaszwiech.binders._"
