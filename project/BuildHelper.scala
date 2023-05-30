import sbt.Keys._
import sbt.{ ThisBuild, _ }

object BuildHelper {

  val scala3 = "3.3.0"

  private val scala3Options = Seq(
    "-encoding",
    "UTF-8",
    "-explain-types",
    "-feature",
    "-language:higherKinds",
    "-Ykind-projector",
    "-unchecked",
    "-deprecation",
    "-Xfatal-warnings"
    // ,"-Yexplicit-nulls"
  )

  def stdSettings(prjName: String) =
    Seq(name := s"$prjName", ThisBuild / scalaVersion := scala3, scalacOptions := scala3Options)
}
