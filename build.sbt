import org.typelevel.idna4s.build._

ThisBuild / tlBaseVersion := "0.1"

val Scala212                    = "2.12.17"
val Scala213                    = "2.13.9"
val Scala3                      = "3.1.3"
def DefaultScalaVersion: String = Scala213

val catsCollectionsV = "0.9.4"
val catsV            = "2.8.0"
val disciplineMunitV = "2.0.0-M3"
val icu4jV           = "71.1"
val literallyV       = "1.1.0"
val munitV           = "1.0.0-M6"
val scalacheckV      = "1.17.0"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion       := Scala213
ThisBuild / developers += tlGitHubDev("isomarcte", "David Strawn")
ThisBuild / licenses  := List(License.MIT)
ThisBuild / startYear := Some(2022)

// Utility

lazy val wildcardImport: SettingKey[Char] =
  settingKey[Char]("Character to use for wildcard imports.")
ThisBuild / wildcardImport := {
  if (tlIsScala3.value) {
    '*'
  } else {
    '_'
  }
}

// Scalafix

// Use the scalafix config which is valid for all versions of Scala if
// scalaVersion is different than the default one. Otherwise, use the full
// scalafix config, which includes Scala version specific rules.
ThisBuild / scalafixConfig := {
  if ((LocalRootProject / scalaVersion).value != DefaultScalaVersion) {
    Some(file(".scalafix-base.conf"))
  } else scalafixConfig.value
}
ThisBuild / scalafixScalaBinaryVersion := {
  if ((LocalRootProject / scalaVersion).value != DefaultScalaVersion) {
    // This is the default according to Scalafix, but the key
    // `scalafixScalaBinaryVersion` isn't actually set in the Global scope, so
    // we can't use `scalafixScalaBinaryVersion.value` here. I believe they
    // are defaulting the the plugin code, rather than in the sbt scope,
    // e.g. `scalafixScalaBinaryVersion.value.?.getOrElse("2.12")`
    "2.12"
  } else {
    (LocalRootProject / scalaBinaryVersion).value
  }
}

ThisBuild / ScalafixConfig / skip := tlIsScala3.value

// Projects

lazy val projectName: String = "idna4s"

lazy val root = tlCrossRootProject
  .aggregate(
    core,
    scalacheck,
    tests
  )
  .settings(name := projectName)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := s"${projectName}-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-collections-core" % catsCollectionsV,
      "org.typelevel" %%% "cats-core"             % catsV
    ),
    libraryDependencies ++= {
      // Needed for macros
      if (tlIsScala3.value) {
        Nil
      } else {
        List(
          "org.typelevel" %%% "literally"     % literallyV,
          "org.scala-lang"  % "scala-reflect" % scalaVersion.value % Provided
        )
      }
    },
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit-scalacheck" % munitV,
      "org.typelevel" %%% "discipline-munit" % disciplineMunitV
    ).map(_ % Test),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := "",
    Compile / sourceGenerators ++= List(
      (Compile / sourceManaged)
        .map(
          UTS46CodeGen.generate
        )
        .taskValue
    )
  )

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("scalacheck"))
  .settings(
    name := s"${projectName}-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalacheckV
    ),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.scalacheck.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.scalacheck.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
  .dependsOn(core)

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .settings(
    name := s"${projectName}-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-munit" % disciplineMunitV,
      "org.typelevel" %%% "cats-laws"        % catsV
    ),
    console / initialCommands := {
      List(
        "cats.",
        "cats.syntax.all.",
        "org.scalacheck.",
        "org.typelevel.idna4s.core.",
        "org.typelevel.idna4s.core.bootstring.",
        "org.typelevel.idna4s.core.syntax.all.",
        "org.typelevel.idna4s.core.uts46.",
        "org.typelevel.idna4s.scalacheck.all."
      ).map(value => s"import ${value}${wildcardImport.value}").mkString("\n")
    },
    consoleQuick / initialCommands := ""
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.ibm.icu" % "icu4j" % icu4jV
    ).map(_ % Test)
  )
  .dependsOn(core % Test, scalacheck % Test)
  .enablePlugins(NoPublishPlugin)
