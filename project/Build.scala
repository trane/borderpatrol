import sbt._
import Keys._

object BorderPatrol extends Build {
  val libVersion = "0.0.1"

  val sharedSettings = Seq(
    version := libVersion,
    organization := "com.lookout",
    scalaVersion := "2.10.4",
    libraryDependencies ++= Seq(
      "com.twitter" %% "twitter-server" % "1.8.0",
      "com.twitter" %% "bijection-core" % "0.7.0",
      "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"
    ),

    scalacOptions ++= Seq("-encoding", "utf8"),
    scalacOptions += "-deprecation",
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    javacOptions in doc := Seq("-source", "1.6"),

    resolvers += "twitter-repo" at "http://maven.twttr.com"
  )


  lazy val borderPatrolCore = Project(
    id = "borderpatrol-core",
    base = file("borderpatrol-core"),
    settings = Project.defaultSettings ++ sharedSettings
  ).settings(
    name := "borderpatrol-core"
  )
}
