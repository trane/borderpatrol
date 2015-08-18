import sbtunidoc.Plugin.UnidocKeys._
import scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages

lazy val buildSettings = Seq(
  organization := "com.lookout",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)

val testDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.scalatest" %% "scalatest" % "2.2.5"
)

val baseSettings = Seq(
  resolvers += "twitter-repo" at "http://maven.twttr.com",
  libraryDependencies ++= Seq(
    "com.twitter" %% "finagle-httpx" % "6.27.0",
    "com.twitter" %% "finagle-memcachedx" % "6.27.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "com.twitter" %% "bijection-core" % "0.8.1",
    "com.twitter" %% "bijection-util" % "0.8.1",
    "io.argonaut" %% "argonaut" % "6.1",
    "org.bouncycastle" % "bcprov-jdk15on" % "1.52",
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  ) ++ testDependencies.map(_ % "test"),
  scalacOptions ++= compilerOptions ++ (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) => Seq("-Ywarn-unused-import")
        case _ => Seq.empty
      }
      ),
  scalacOptions in (Compile, console) := compilerOptions :+ "-Yrepl-class-based",
  wartremoverWarnings in (Compile, compile) ++= Warts.allBut(Wart.FinalCaseClass, Wart.NoNeedForMonad, Wart.Throw, Wart.Null, Wart.Nothing, Wart.DefaultArguments)
)

lazy val publishSettings = Seq(
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("https://github.com/lookout/borderpatrol")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/lookout/borderpatrol"),
      "scm:git:git@github.com:lookout/borderpatrol.git"
    )
  ),
  bintrayRepository := "borderpatrol",
  publishMavenStyle := true,
  bintrayOrganization := Some("lookout")
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {}
)

lazy val allSettings = baseSettings ++ buildSettings ++ publishSettings

lazy val docSettings = site.settings ++ ghpages.settings ++ unidocSettings ++ Seq(
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "docs"),
  git.remoteRepo := s"git@github.com:lookout/borderpatrol.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject
)

lazy val root = project.in(file("."))
  .settings(moduleName := "borderpatrol")
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublish)
  .settings(
    initialCommands in console :=
      """
        |import com.lookout.borderpatrol.sessionx._
        |import com.lookout.borderpatrol.auth._
      """.stripMargin
    )
  .aggregate(core, example, security, auth)
  .dependsOn(core)

lazy val core = project
  .settings(moduleName := "borderpatrol-core")
  .settings(allSettings)
  .settings(coverageExcludedPackages := "com\\.lookout\\.borderpatrol\\.auth\\..*")
  .dependsOn(test % "test")

lazy val test = project
    .settings(moduleName := "borderpatrol-test")
    .settings(allSettings)
    .settings(coverageExcludedPackages := "com\\.lookout\\.borderpatrol\\.test\\..*")
    .settings(libraryDependencies ++= testDependencies)

lazy val example = project
  .settings(resolvers += Resolver.sonatypeRepo("snapshots"))
  .settings(moduleName := "borderpatrol-example")
  .settings(allSettings)
  .settings(noPublish)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core" % "0.8.0",
      "com.github.finagle" %% "finch-argonaut" % "0.8.0"
    )
  )
  .disablePlugins(JmhPlugin)
  .dependsOn(core, test % "test")

lazy val security = project
  .settings(moduleName := "borderpatrol-security")
  .settings(allSettings)
  .dependsOn(core, test % "test")

lazy val auth = project
  .settings(moduleName := "borderpatrol-auth")
  .settings(allSettings)
  .dependsOn(core, test % "test")

lazy val server = project
  .settings(moduleName := "borderpatrol-server")
  .settings(allSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core" % "0.8.0"
    )
  )
  .dependsOn(core, test % "test")
