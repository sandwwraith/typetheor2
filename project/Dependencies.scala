import sbt._

object Dependencies {
  lazy val scalaZ = Seq("org.scalaz" %% "scalaz-core" % "7.2.20", "org.scalaz" %% "scalaz-effect" % "7.2.20")
  lazy val parbolied = "org.parboiled" %% "parboiled" % "2.1.4"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
}
