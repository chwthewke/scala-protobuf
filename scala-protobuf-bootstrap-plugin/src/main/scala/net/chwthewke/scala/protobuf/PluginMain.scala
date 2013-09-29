package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse

object PluginMain extends PluginDriver {

  override def log(s: String): Unit = { System.err.println(s); super.log(s) }

  def main(arg: Array[String]): Unit = run(Plugin.process)

}

