val _scalaVersion = "2.12.4"

resolvers in ThisBuild += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

val root = (project in file("."))
  .settings(name := "training",
    scalaVersion := _scalaVersion,
    scalaVersion in ThisBuild := _scalaVersion,
    libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.4" % "test"
    )
  )
