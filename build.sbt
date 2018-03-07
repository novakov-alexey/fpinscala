scalaVersion in ThisBuild := "2.12.3"

val commonSettings = Seq(
  scalaVersion := "2.12.3"
)

lazy val root = (project in file("."))
  .aggregate(exercises)
//  .aggregate(answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )
lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
