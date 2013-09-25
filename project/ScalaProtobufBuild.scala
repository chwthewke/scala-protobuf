import sbt._
import sbt.Keys._

import sbtprotobuf.{ProtobufPlugin => PB}

object ScalaProtobufBuild extends Build {

  val ScalaProtobufDefaults = Seq(
    organization := "net.chwthewke",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.2")

  // required for bootstrapping
  val protobufJava = "com.google.protobuf" % "protobuf-java" % "2.5.0"

  lazy val scalaProtobufParent = Project(
    id = "scala-protobuf-parent",
    base = file("."),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf-parent"))
    .aggregate(scalaProtobufRuntime, scalaProtobufPlugin, pluginRuntime)

  lazy val scalaProtobufRuntime = Project(
    id = "scala-protobuf",
    base = file("scala-protobuf"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf")
  )

  lazy val scalaProtobufPlugin = Project(
    id = "scala-protobuf-plugin",
    base = file("scala-protobuf-plugin"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf-plugin")
  ).dependsOn(pluginRuntime)

  lazy val pluginRuntime = Project(
    id = "scala-protobuf-plugin-runtime-java",
    base = file("scala-protobuf-plugin-runtime-java"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      PB.protobufSettings ++ Seq(
      name := "scala-protobuf-plugin-runtime-java",
      libraryDependencies += protobufJava,
      version in PB.protobufConfig := "2.5.0",
      javaSource in PB.protobufConfig <<= baseDirectory {_ / "generated-src" / "protobuf"})
  )

}
