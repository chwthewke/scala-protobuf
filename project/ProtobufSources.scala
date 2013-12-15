package net.chwthewke.sbt.protobuf

import sbt._
import sbt.Keys._
import sbtprotobuf.{ProtobufPlugin => PB}
import scala.util.Try

object ProtobufSources {

  val config = sbt.config("protobuf-sources")

  val suffix = SettingKey[String]("protobuf-sources-suffix",
    "The suffix to append to the java_package of the protobuf sources.")

  val filter = TaskKey[Seq[File]]("protobuf-sources-filter",
    "Copy the protobuf sources to this project, appending the suffix to their java_package.")


  val settings = inConfig(config)(Seq(
    suffix := "ref",
    filter := rewriteAllSources(
      (sourceDirectory in config).value,
      (sourceDirectory in PB.protobufConfig).value,
      (suffix in config).value)
  )) ++ PB.protobufSettings ++ Seq(
    sourceDirectory in PB.protobufConfig := sourceManaged.value / "protobuf",
    PB.generate in PB.protobufConfig <<=
      (PB.generate in PB.protobufConfig).dependsOn(filter in config))


  private val optionRegex = """option\s*java_package\s*=\s*"(.*)";""".r

  private def optionReplacement(suffix: String) = """option java_package = "$1.""" + suffix + """";"""

  private def rewriteAllSources(source: File, target: File, suffix: String): Seq[File] = {
    IO.createDirectory(target)
    (source ** "*.proto").get.map(rewriteSource(_, source, target, suffix))
  }

  private def rewriteSource(sourceProto: File, sourceDirectory: File, targetDirectory: File, suffix: String): File = {
    val targetFile: File =
      IO.resolve(targetDirectory, file(IO.relativize(sourceDirectory, sourceProto).get))
    rewriteJavaPackage(sourceProto, targetFile, suffix)
    targetFile
  }

  private def rewriteJavaPackage(in: File, out: File, suffix: String): Unit = tryRewriteJavaPackage(in, out, suffix).get

  private def tryRewriteJavaPackage(in: File, out: File, suffix: String): Try[Unit] = {
    for {
      contentIn <- Try(IO.readLines(in))
      contentOut = contentIn map (rewrite(_, suffix))
      written <- Try(IO.writeLines(out, contentOut))
    } yield written
  }

  private def rewrite(source: String, suffix: String): String = optionRegex.replaceAllIn(source, optionReplacement(suffix))
}
