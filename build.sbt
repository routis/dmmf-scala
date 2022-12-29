import BuildHelper.stdSettings

ThisBuild / scalaVersion := "3.2.1"
ThisBuild / organization := "io.gitlab.routis.dmmf"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

lazy val orderTaking = (project in file("."))
  .aggregate(orderTakingDomain, orderTakingInfrastructure)
  .settings(stdSettings("dmmf"))
  .settings(publishArtifact := false, publish / skip := true, publishLocal / skip := true)

lazy val orderTakingDomain = module("order-taking-domain", "orderTaking/domain/")
  .settings(libraryDependencies ++= Dependencies.coreDeps, testFrameworks += Dependencies.coreTestingFramework)

lazy val orderTakingInfrastructure =
  module("order-taking-infrastructure", "orderTaking/infrastructure")
    .aggregate(orderTakingInfrastructureDB)
    .settings(publishArtifact := false, publish / skip := true, publishLocal / skip := true)

lazy val orderTakingInfrastructureDB =
  module("order-taking-infrastructure-db", "orderTaking/infrastructure/db")
    .dependsOn(`orderTakingDomain` % "compile->compile;test->test")
    .settings(
      scalacOptions -= "-Yexplicit-nulls",
      libraryDependencies ++= Dependencies.dbDeps,
      excludeDependencies ++= Seq(
        ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13"),
        ExclusionRule("com.lihaoyi", "sourcecode_2.13"),
        ExclusionRule("com.lihaoyi", "fansi_2.13"),
        ExclusionRule("com.lihaoyi", "pprint_2.13")
      ),
      testFrameworks += Dependencies.dbTestingFramework
    )

def module(moduleName: String, fileName: String): Project =
  Project(moduleName, file(fileName))
    .settings(stdSettings(moduleName))
