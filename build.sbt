import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "tt",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "typetheory",
//    libraryDependencies ++= scalaZ,
    libraryDependencies += parbolied,
    libraryDependencies += scalaTest % Test
  )
