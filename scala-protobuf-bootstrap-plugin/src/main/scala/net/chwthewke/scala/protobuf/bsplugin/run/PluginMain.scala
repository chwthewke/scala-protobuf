package net.chwthewke.scala.protobuf.bsplugin.run

import net.chwthewke.scala.protobuf.bsplugin.Plugin

object PluginMain extends PluginDriver {

  override def log(s: String): Unit = { System.err.println(s); super.log(s) }

  def main(arg: Array[String]): Unit = run(Plugin.process)

}

