import sbt._
import sbt.Keys._

import sbtprotobuf.{ProtobufPlugin => PB}

object ScalaProtobufBuild extends Build {

  val ScalaProtobufDefaults = Seq(
    organization := "net.chwthewke",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"))

  // required for bootstrapping
  val protobufJava = "com.google.protobuf" % "protobuf-java" % "2.5.0"

  lazy val scalaProtobufParent = Project(
    id = "scala-protobuf-parent",
    base = file("."),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf-parent"))
    .aggregate(scalaProtobufRuntime, scalaProtobufBootstrapPlugin)

  lazy val scalaProtobufRuntime = Project(
    id = "scala-protobuf",
    base = file("scala-protobuf"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf")
  )

  lazy val scalaProtobufBootstrapPlugin = Project(
    id = "scala-protobuf-bootstrap-plugin",
    base = file("scala-protobuf-bootstrap-plugin"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
        PB.protobufSettings ++ Seq(
        name := "scala-protobuf-bootstrap-plugin",
        mainClass := Some("net.chwthewke.scala.protobuf.PluginMain"),
        libraryDependencies += protobufJava,
        version in PB.protobufConfig := "2.5.0",
        PB.includePaths in PB.protobufConfig += (sourceDirectory in Compile).value / "protobuf-inc",
        javaSource in PB.protobufConfig <<= baseDirectory {_ / "generated-src" / "protobuf"})
  )

  lazy val testToPython = Project(
    id = "scala-protobuf-gen-python",
    base = file("scala-protobuf-gen-python"),
    settings = Project.defaultSettings ++
      PB.protobufSettings ++ Seq(
        name := "scala-protobuf-gen-python",
        version in PB.protobufConfig := "2.5.0",
        PB.plugin := "python"
    )
  )

}
