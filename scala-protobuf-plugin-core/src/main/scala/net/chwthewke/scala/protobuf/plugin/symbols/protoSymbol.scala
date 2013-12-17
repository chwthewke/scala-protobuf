package net.chwthewke.scala.protobuf.plugin.symbols

import net.chwthewke.scala.protobuf.plugin.interface._
import net.chwthewke.scala.protobuf.plugin.syntax._

sealed abstract class ProtoSymbol {
  def file: FileDescriptor
  def source: Option[Location]

  def fqn: String

  def mkString: String
}

case class MessageSymbol(
  file: FileDescriptor,
  source: Option[Location],
  fqn: String,
  descriptor: Descriptor,
  cls: String,
  javaFqn: String)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls ($javaFqn)"
}

case class FileSymbol(
  file: FileDescriptor,
  source: Option[Location],
  fqn: String,
  descriptor: FileDescriptor,
  obj: String,
  pkg: String)
  extends ProtoSymbol {

  def mkString = s"${file.name} -> object $obj"
}

case class EnumSymbol(
  file: FileDescriptor,
  source: Option[Location],
  fqn: String,
  descriptor: EnumDescriptor,
  cls: String,
  javaFqn: String,
  values: Map[EnumValueDescriptor, String])
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls ($javaFqn), values: ${
    values.map { case (v, s) => s"${v.name} -> object $s" }.mkString(", ")
  }"
}

case class FieldSymbol(
  file: FileDescriptor,
  source: Option[Location],
  fqn: String,
  descriptor: FieldDescriptor,
  defn: String)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> member $defn"

}

