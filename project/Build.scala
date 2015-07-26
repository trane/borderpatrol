import sbt.Keys._
import sbt._

object BorderPatrol extends Build {
  val libVersion = "0.1.0"
  import bintray.BintrayKeys._
  val twitter_server = "1.11.0"
  val finagle_version = "6.26.0"

  val sharedSettings = Seq(
    version := libVersion,
    organization := "com.lookout",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "com.twitter" %% "finagle-memcachedx" % finagle_version,
      "com.twitter" %% "finagle-httpx" % finagle_version,
      "com.twitter" %% "bijection-core" % "0.8.1",
      "com.twitter" %% "bijection-util" % "0.8.1",
      "io.argonaut" %% "argonaut" % "6.1",
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "org.scalamock" %% "scalamock-scalatest-support" % "3.2.1" % "test",
      "org.skife.com.typesafe.config" % "typesafe-config" % "0.3.0",
      "org.bouncycastle" % "bcprov-jdk15on" % "1.52"
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
    name := "borderpatrol-core",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayRepository := "borderpatrol"
  )

  lazy val borderPatrolExample = Project(
    id = "borderpatrol-example",
    base = file("borderpatrol-example"),
    settings = Defaults.coreDefaultSettings ++ sharedSettings
  ).settings(
      name := "borderpatrol-example",
      libraryDependencies ++= Seq(
        "com.twitter" %% "twitter-server" % twitter_server,
        "com.github.finagle" %% "finch-core" % "0.8.0-SNAPSHOT" changing(),
        "com.github.finagle" %% "finch-argonaut" % "0.8.0-SNAPSHOT" changing()
      ),
      resolvers ++= Seq(
        Resolver.sonatypeRepo("snapshots")
      )
  ).dependsOn(borderPatrolCore)

  lazy val borderPatrol = Project(
    id = "borderpatrol",
    base = file("."),
    settings = Defaults.coreDefaultSettings ++ sharedSettings
  ) aggregate (borderPatrolCore, borderPatrolExample)
}
