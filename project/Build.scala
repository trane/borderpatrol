import sbt.Keys._
import sbt._

object BorderPatrol extends Build {
  val libVersion = "0.1.0"
  val twitter_server = "1.9.0"

  val sharedSettings = Seq(
    version := libVersion,
    organization := "com.lookout",
    scalaVersion := "2.10.4",
    libraryDependencies ++= Seq(
      "com.twitter" %% "twitter-server" % twitter_server,
      "com.twitter" %% "bijection-core" % "0.7.0",
      "io.argonaut" %% "argonaut" % "6.0.4",
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "org.scalamock" %% "scalamock-scalatest-support" % "3.2.1" % "test",
      "org.skife.com.typesafe.config" % "typesafe-config" % "0.3.0",
      "org.bouncycastle" % "bcprov-jdk15on" % "1.51"
    ),
    scalacOptions ++= Seq("-encoding", "utf8"),
    scalacOptions += "-deprecation",
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    javacOptions in doc := Seq("-source", "1.6"),

    resolvers += "twitter-repo" at "http://maven.twttr.com",

    // This is bad news for things like com.twitter.util.Time
    parallelExecution in Test := false,
    fork := false

  )

  lazy val borderPatrolCore = Project(
    id = "borderpatrol-core",
    base = file("borderpatrol-core"),
    settings = Defaults.coreDefaultSettings ++ sharedSettings
  ).settings(
    name := "borderpatrol-core"
  )
}
