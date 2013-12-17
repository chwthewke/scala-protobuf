package net.chwthewke.scala.protobuf.plugin.symbols

trait Names {
  def message(s: String) = s.split("_").map(_.capitalize).mkString("")

  def enumValue(s: String) = s.split("(?<=[a-z0-9])(?=[A-Z])").mkString("_").toUpperCase

  def field(s: String) = decapitalize(message(s))

  private def decapitalize(s: String) = if (s.isEmpty) s else s(0).toLower + s.substring(1)
}

object Names extends Names
