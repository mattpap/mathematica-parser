import sbt._
import Keys._

object SymPy extends Build {
    val scalaOpts = Seq("-deprecation", "-unchecked", "-feature")
    val languageOpts = Seq("implicitConversions")

    override lazy val settings = super.settings ++ Seq(
        version := "0.0.1-SNAPSHOT",
        organization := "org.sympy",
        scalaVersion := "2.10.0-M4",
        scalacOptions ++= scalaOpts ++ languageOpts.map("-language:" + _),
        shellPrompt := { state =>
            "sympy (%s)> ".format(Project.extract(state).currentProject.id)
        },
        resolvers ++= Seq(
            "Releases" at "http://oss.sonatype.org/content/repositories/releases",
            "Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        )
    )

    val specs2 = "org.specs2" %% "specs2" % "1.11" % "test"
    val jansi = "org.fusesource.jansi" % "jansi" % "1.8"

    lazy val sympySettings = settings ++ Seq(
        libraryDependencies ++= Seq(specs2, jansi),
        initialCommands := """import org.sympy.parsing.mathematica._"""
    )

    lazy val sympy = Project(id="sympy", base=file(".")) settings(sympySettings: _*)

    override def projects = Seq(sympy)
}
