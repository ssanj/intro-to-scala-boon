name := "intro-to-scala-boon"

version := "0.1"

libraryDependencies ++= Seq(
  "net.ssanj" %% "boon" % "1.2.0" % Test,
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

testFrameworks := Seq(new TestFramework("boon.sbt.BoonFramework"), sbt.TestFrameworks.ScalaTest)

scalaVersion := "2.12.13"

scalacOptions ++= Seq(
  "-Xfatal-warnings"
)

