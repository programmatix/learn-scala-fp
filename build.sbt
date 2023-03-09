ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "learn-scala-fp"
  )

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.17.0",
  "org.scalactic" %% "scalactic" % "3.2.15",
  "org.scalatest" %% "scalatest" % "3.2.15",
  "com.softwaremill.quicklens" %% "quicklens" % "1.9.0"
)