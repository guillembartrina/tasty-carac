
ThisBuild / scalaVersion := "3.3.0"

lazy val root = project.in(file("."))
  .settings(
    name := "tasty-carac",
    libraryDependencies ++= Seq(
      //"io.get-coursier" %% "coursier" % "2.1.3",
      "ch.epfl.lamp" %% "datalog" % "0.1",
      "ch.epfl.scala" %% "tasty-query" % "1.0.0",

      "org.scala-lang" %% "scala3-compiler" % "3.3.0" % Test,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    envVars += {
      "TASTYCARAC_DEFAULTCLASSPATH" -> Attributed.data((Compile / fullClasspath).value).map(_.getAbsolutePath).mkString(";")
    },
    fork := true
  )
  .dependsOn(dummy)

lazy val dummy = project.in(file("dummy"))
  .settings(
    publish / skip := true
  )
