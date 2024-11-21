import sbt._

object Dependencies {

  private lazy val zio = Seq(
    "dev.zio"             %% "zio"               % versions.zio,
    "dev.zio"             %% "zio-streams"       % versions.zio,
    "dev.zio"             %% "zio-prelude"       % versions.prelude,
    "dev.zio"             %% "zio-test"          % versions.zio       % "test",
    "dev.zio"             %% "zio-test-sbt"      % versions.zio       % "test",
    "dev.zio"             %% "zio-test-magnolia" % versions.zio       % "test",
    "com.github.javafaker" % "javafaker"         % versions.javafaker % "test"
  )

  private lazy val jodaMoney = Seq("org.joda" % "joda-money" % versions.jodaMoney)

  private lazy val mariadb = Seq("org.mariadb.jdbc" % "mariadb-java-client" % versions.mariadb)
  // private lazy val zioQuill =
  //  Seq("io.getquill" %% "quill-jdbc-zio" % versions.zio_quill withSources ())

  lazy val coreDeps: Seq[ModuleID] = zio ++ jodaMoney
  lazy val coreTestingFramework    = new TestFramework("zio.test.sbt.ZTestFramework")
  // lazy val dbDeps: Seq[ModuleID]   = zioQuill ++ mariadb
  lazy val dbDeps: Seq[ModuleID] = mariadb

  lazy val dbTestingFramework = new TestFramework("zio.test.sbt.ZTestFramework")

  object versions {

    val zio = "2.0.21"
    // val zio_quill    = "3.12.0.Beta1.7"
    val mariadb   = "2.7.10"
    val prelude   = "1.0.0-RC35"
    val jodaMoney = "1.0.4"
    val javafaker = "1.0.2"
  }

}
