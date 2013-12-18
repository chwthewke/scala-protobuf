import sbt._
import sbt.Keys._

import sbtprotobuf.{ ProtobufPlugin => PB }
import sbtprotobuf.ProtobufPlugin.ProtocPlugin
import sbtbuildinfo.Plugin._
import sbtassembly.Plugin._
import AssemblyKeys._

import net.chwthewke.sbt.protobuf.{ ProtobufSources => PS }

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
        scalaProtobufRuntimeTests,
        scalaProtobufBootstrapPlugin,
        scalaProtobufPlugin,
        scalaProtobufTestProtocol,
        scalaProtobufReferenceTestProtocol)

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

  lazy val scalaProtobufRuntimeTests = Project(
    id = "scala-protobuf-test",
    base = file("scala-protobuf-test"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults
  ).settings(
      name := "scala-protobuf-test",
      libraryDependencies ++= Seq(scalatest, scalacheck)
    ).dependsOn(
        scalaProtobufRuntime,
        scalaProtobufTestProtocol % "test",
        scalaProtobufReferenceTestProtocol % "test"
      )

  lazy val scalaProtobufPluginCore = Project(
    id = "scala-protobuf-plugin-core",
    base = file("scala-protobuf-plugin-core")
  ).settings(
      name := "scala-protobuf-plugin-core",
      libraryDependencies ++= Seq(scalaz)
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
      mainClass := Some("net.chwthewke.scala.protobuf.plugin.PluginMain"),
      libraryDependencies ++= Seq(scalaz)
    ).dependsOn(
        scalaProtobufRuntime,
        scalaProtobufPluginCore
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
      mainClass := Some("net.chwthewke.scala.protobuf.plugin.google.PluginMain"),
      libraryDependencies ++= Seq(scalaz)
    ).dependsOn(
        scalaProtobufPluginCore
      )

  lazy val scalaProtobufTestProtocol = Project(
    id = "scala-protobuf-test-protocol",
    base = file("scala-protobuf-test-protocol"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      scalaProtobufSettings
  ).settings(
      name := "scala-protobuf-test-protocol"
    ).dependsOn(
        scalaProtobufRuntime
      )

  lazy val scalaProtobufReferenceTestProtocol = Project(
    id = "scala-protobuf-reference-test-protocol",
    base = file("scala-protobuf-reference-test-protocol"),
    settings = Project.defaultSettings ++
      PS.settings ++
      protobufEclipseSettings
  ).settings(
      name := "scala-protobuf-reference-test-protocol",
      sourceDirectory in PS.config := (sourceDirectory in (scalaProtobufTestProtocol, PB.protobufConfig)).value,
      version in PB.protobufConfig := "2.5.0"
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
