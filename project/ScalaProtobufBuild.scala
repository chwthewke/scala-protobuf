import java.nio.charset.Charset
import java.nio.file.Files
import sbt._
import sbt.Keys._

import sbtprotobuf.{ProtobufPlugin => PB}
import sbtbuildinfo.Plugin._
import sbtassembly.Plugin._
import AssemblyKeys._


object ScalaProtobufBuild extends Build {

  val ScalaProtobufDefaults = Seq(
    organization := "net.chwthewke",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"))

  // required for bootstrapping
  val protobufJava = "com.google.protobuf" % "protobuf-java" % "2.5.0"

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.3"

  val treehugger = "com.eed3si9n" %% "treehugger" % "0.3.0"

  val myBuildInfoSettings = buildInfoSettings ++ Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := "net.chwthewke.scala.protobuf",
    managedSourceDirectories in Compile += (sourceManaged in Compile).value / "sbt-buildinfo"
  )

  lazy val scalaProtobufParent = Project(
    id = "scala-protobuf-parent",
    base = file("."),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults :+
      (name := "scala-protobuf-parent"))
    .aggregate(scalaProtobufRuntime, scalaProtobufBootstrapPlugin, scalaProtobufPlugin)

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
      ScalaProtobufDefaults ++
      PB.protobufSettings ++
      myBuildInfoSettings
  ).settings(
    name := "scala-protobuf-plugin",
    libraryDependencies += scalaz,
    version in PB.protobufConfig := "2.5.0",
    PB.pluginExecutable in PB.protobufConfig :=
      Some((launchBatName in assembly in scalaProtobufBootstrapPlugin).value),
    PB.plugin in PB.protobufConfig := "scala",
    PB.generate in PB.protobufConfig <<=
      (PB.generate in PB.protobufConfig).dependsOn(assembly in scalaProtobufBootstrapPlugin)
  ).dependsOn(
    scalaProtobufRuntime
  )

  lazy val scalaProtobufBootstrapPlugin = Project(
    id = "scala-protobuf-bootstrap-plugin",
    base = file("scala-protobuf-bootstrap-plugin"),
    settings = Project.defaultSettings ++
      ScalaProtobufDefaults ++
      PB.protobufSettings ++
      myBuildInfoSettings ++
      assemblySettings ++
      launcherSettings
  ).settings(
    name := "scala-protobuf-bootstrap-plugin",
    mainClass := Some("net.chwthewke.scala.protobuf.PluginMain"),
    libraryDependencies ++= Seq(protobufJava, scalaz, treehugger),
    version in PB.protobufConfig := "2.5.0",
    PB.includePaths in PB.protobufConfig += (sourceDirectory in Compile).value / "protobuf-inc"
  )

  lazy val launchBatName: SettingKey[File] = settingKey[File]("Location of launcher .bat")

  lazy val launchBat: TaskKey[File] = taskKey[File]("Generate .bat to launch an assembly jar")

  lazy val launcherSettings = Seq(
    launchBatName in assembly := target.value / "launch.bat",
    launchBat := launchBat(
      (launchBatName in assembly).value,
      (outputPath in assembly).value,
      javaHome.value,
      streams.value.log),
    assembly <<= assembly.dependsOn(launchBat in assembly))

  def launchBat(batFile: File, jarFile: File, javaHome: Option[File], log: Logger) = {
    import scala.collection.JavaConverters.asJavaIterableConverter

    val relativeJar: String =
      batFile.toPath.getParent.relativize(jarFile.toPath).toString

    val javaHomeStr = javaHome.map(_.absolutePath + "\\").getOrElse("")

    val lines = Seq(
      "@echo off",
      s"rem launches $relativeJar",
      "cd %~dp0",
      s"${javaHomeStr}java -jar $relativeJar"
    )

    Files.write(batFile.toPath, lines.asJava, Charset.forName("UTF-8"))

    batFile
  }


}
