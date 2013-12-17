package net.chwthewke.scala.protobuf.plugin.google

import net.chwthewke.scala.protobuf.plugin.{Plugin, interface}

class PluginStart(val io: interface.IO) extends PluginDriver {

  override def log(s: String): Unit = { System.err.println(s); super.log(s) }

  def main(arg: Array[String]): Unit = run(Plugin.process)

}
