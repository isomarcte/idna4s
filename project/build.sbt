// ThisBuild / scalacOptions ++= List("-deprecation", "-encoding", "UTF-8", "-feature", "-unchecked", "-Yno-adapted-args", "-Ywarn-unused-import", "-Ywarn-numeric-widen", "-Xlint:-unused","_", "-Xlint", "-Ywarn-dead-code", "-Ypartial-unification", "-Ybackend-parallelism", "16", "-language:_")

libraryDependencies ++= List(
  "com.eed3si9n" %% "treehugger" % "0.4.4",
  "org.typelevel" %% "cats-core" % "2.8.0"
)
