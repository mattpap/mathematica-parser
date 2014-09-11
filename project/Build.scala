import sbt._
import Keys._

object Dependencies {
    val isScala_2_10 = Def.setting {
        scalaVersion.value.startsWith("2.10")
    }

    def scala_2_10(moduleID: ModuleID) =
        Def.setting { if (isScala_2_10.value) Seq(moduleID) else Seq.empty }

    def scala_2_11_+(moduleID: ModuleID) =
        Def.setting { if (!isScala_2_10.value) Seq(moduleID) else Seq.empty }

    val parser_combinators = scala_2_11_+("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2")

    val parboiled2 = "org.parboiled" %% "parboiled" % "2.0.1"

    val scopt = "com.github.scopt" %% "scopt" % "3.2.0"

    val specs2 = "org.specs2" %% "specs2" % "2.4" % Test
}

object MathematicaParser extends Build {
    override lazy val settings = super.settings ++ Seq(
        organization := "org.refptr",
        version := "0.2-SNAPSHOT",
        description := "Mathematica language parser",
        homepage := Some(url("http://github.com/mattpap/mathematica-parser")),
        licenses := Seq("MIT-style" -> url("http://www.opensource.org/licenses/mit-license.php")),
        scalaVersion := "2.11.2",
        crossScalaVersions := Seq("2.10.4", "2.11.2"),
        scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_"),
        scalacOptions in (Compile, doc) := Seq("-groups", "-implicits"),
        shellPrompt := { state =>
            "refptr (%s)> ".format(Project.extract(state).currentProject.id)
        },
        cancelable := true,
        resolvers += Resolver.sonatypeRepo("releases")
    )

    lazy val publishSettings = Seq(
        publishTo := {
            val nexus = "https://oss.sonatype.org/"
            if (isSnapshot.value)
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },
        publishMavenStyle := true,
        publishArtifact in Test := false,
        pomIncludeRepository := { _ => false },
        pomExtra := (
            <scm>
                <url>https://github.com/mattpap/mathematica-parser</url>
                <connection>scm:git:https://github.com/mattpap/mathematica-parser.git</connection>
            </scm>
            <developers>
                <developer>
                    <id>mattpap</id>
                    <name>Mateusz Paprocki</name>
                    <url>mateuszpaprocki.pl</url>
                </developer>
            </developers>
        ),
        credentials ++= {
            (for {
                username <- sys.env.get("SONATYPE_USERNAME")
                password <- sys.env.get("SONATYPE_PASSWORD")
            } yield {
                Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
            }) orElse {
                val path = Path.userHome / ".sonatype" / "credentials"
                if (path.exists) Some(Credentials(path)) else None
            } toList
        }
    )

    import Dependencies._

    lazy val mathematicaParserSettings = Defaults.coreDefaultSettings ++ publishSettings ++ Seq(
        libraryDependencies ++= parser_combinators.value ++ Seq(parboiled2, scopt, specs2),
        initialCommands := """import org.refptr.parsing.mathematica._"""
    )

    lazy val mathematica_parser = project in file(".") settings(mathematicaParserSettings: _*)

    override def projects = Seq(mathematica_parser)
}
