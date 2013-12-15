import sbt._
import sbt.Keys._
import sbtassembly.Plugin.AssemblyKeys._

object ProtocPluginLauncher {
  lazy val launcher: SettingKey[File] = settingKey[File]("Location of launcher .bat")

  lazy val generateLauncher: TaskKey[File] = taskKey[File]("Generate .bat to launch an assembly jar")

  lazy val settings = Seq(
    launcher in assembly := target.value / "launch.bat",
    generateLauncher := launchBat(
      (launcher in assembly).value,
      (outputPath in assembly).value,
      javaHome.value,
      streams.value.log),
    assembly <<= assembly.dependsOn(generateLauncher in assembly))

  def launchBat(batFile: File, jarFile: File, javaHome: Option[File], log: Logger) = {

    val relativeJar: String =
      batFile.toPath.getParent.relativize(jarFile.toPath).toString

    val javaHomeStr = javaHome.map(_.absolutePath + "\\").getOrElse("")

    val lines = Seq(
      "@echo off",
      s"rem launches $relativeJar",
      "cd %~dp0",
      s"${javaHomeStr}java -jar $relativeJar"
    )

    IO.writeLines(batFile, lines)

    batFile
  }
}
