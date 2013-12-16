package net.chwthewke.scala.protobuf.bsplugin.run

import net.chwthewke.scala.protobuf.bsplugin.Plugin
import net.chwthewke.scala.protobuf.bsplugin.interface

object PluginMain extends PluginDriver {

  override def log(s: String): Unit = { System.err.println(s); super.log(s) }

  def main(arg: Array[String]): Unit = run(Plugin.process)

  val io = interface.google

}

