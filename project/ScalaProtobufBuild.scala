import sbt._
import sbt.Keys._

import sbtprotobuf.{ProtobufPlugin => PB}
import sbtprotobuf.ProtobufPlugin.ProtocPlugin
import sbtbuildinfo.Plugin._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalaProtobufBuild extends Build {

  val ScalaProtobufDefaults = Seq(
    organization := "net.chwthewke",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"))

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.4"

  val scalatest = "org.scalatest" %% "scalatest" % "2.0" % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  
  val myBuildInfoSettings = buildInfoSettings ++ Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := "net.chwthewke.scala.protobuf",
    unmanagedSourceDirectories in Compile += (sourceManaged in Compile).value / "sbt-buildinfo"
  )

  lazy val scalaProtobufParent = Project(
    id = "scala-protobuf-parent",
    base = file("."),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults
  ).settings(
    name := "scala-protobuf-parent"
  ).aggregate(
    scalaProtobufRuntime,
    scalaProtobufBootstrapPlugin,
    scalaProtobufPlugin)

  lazy val scalaProtobufRuntime = Project(
    id = "scala-protobuf",
    base = file("scala-protobuf"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      scalaProtobufSettings
  ).settings(
    name := "scala-protobuf",
    libraryDependencies ++= Seq(scalatest, scalacheck)
  )

  lazy val scalaProtobufPlugin = Project(
    id = "scala-protobuf-plugin",
    base = file("scala-protobuf-plugin"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      scalaProtobufSettings ++
      includeProtobufSettings ++
      myBuildInfoSettings
  ).settings(
    name := "scala-protobuf-plugin",
    libraryDependencies ++= Seq(scalaz)
  ).dependsOn(
    scalaProtobufRuntime
  )

  lazy val scalaProtobufBootstrapPlugin = Project(
    id = "scala-protobuf-bootstrap-plugin",
    base = file("scala-protobuf-bootstrap-plugin"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      baseProtobufSettings ++
      includeProtobufSettings ++
      myBuildInfoSettings ++
      assemblySettings ++
      ProtocPluginLauncher.settings
  ).settings(
    name := "scala-protobuf-bootstrap-plugin",
    mainClass := Some("net.chwthewke.scala.protobuf.bsplugin.run.PluginMain"),
    libraryDependencies ++= Seq(scalaz)
  )

  def protobufEclipseSettings = Seq(unmanagedSourceDirectories in Compile += (javaSource in PB.protobufConfig).value)

  def baseProtobufSettings = PB.protobufSettings ++ Seq(
    version in PB.protobufConfig := "2.5.0") ++
    protobufEclipseSettings

  def includeProtobufSettings =
    Seq(PB.includePaths in PB.protobufConfig += (sourceDirectory in Compile).value / "protobuf-inc")


  def scalaProtobufSettings = baseProtobufSettings ++ Seq(
    PB.plugins in PB.protobufConfig := Seq(
      ProtocPlugin(
        "scala",
        (sourceManaged in Compile).value / "compiled_protobuf",
        Some((ProtocPluginLauncher.launcher in assembly in scalaProtobufBootstrapPlugin).value),
        _ ** "*.scala")),
    PB.generate in PB.protobufConfig <<=
      (PB.generate in PB.protobufConfig).dependsOn(assembly in scalaProtobufBootstrapPlugin)
  )


}
