import sbt._
import Keys._

object SymPy extends Build {
    override lazy val settings = super.settings ++ Seq(
        version := "0.1",
        organization := "org.sympy",
        scalaVersion := "2.10.2",
        scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_"),
        shellPrompt := { state =>
            "sympy (%s)> ".format(Project.extract(state).currentProject.id)
        },
        resolvers ++= Seq(
            "Releases" at "http://oss.sonatype.org/content/repositories/releases",
            "Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        )
    )

    val specs2 = "org.specs2" %% "specs2" % "2.1.1" % "test"

    lazy val projectSettings = Project.defaultSettings ++ Seq(
        libraryDependencies += specs2,
        initialCommands := """import org.sympy.parsing.mathematica._"""
    )

    lazy val MathematicaParser = Project(id="MathematicaParser", base=file(".")) settings(projectSettings: _*)

    override def projects = Seq(MathematicaParser)
}
