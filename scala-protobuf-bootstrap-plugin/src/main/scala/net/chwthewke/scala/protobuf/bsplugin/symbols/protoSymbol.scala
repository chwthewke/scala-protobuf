package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.SourceCodeInfo.Location
import net.chwthewke.scala.protobuf.bsplugin.syntax._

sealed abstract class ProtoSymbol {
  def file: FileDescriptorProto
  def source: Option[Location]

  def fqn: String

  def mkString: String
}

case class MessageSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: DescriptorProto,
  cls: String,
  javaFqn: String)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls ($javaFqn)"
}

case class FileSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: FileDescriptorProto,
  obj: String,
  pkg: String)
  extends ProtoSymbol {

  def mkString = s"${file.name} -> object $obj"
}

case class EnumSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: EnumDescriptorProto,
  cls: String,
  javaFqn: String,
  values: Map[EnumValueDescriptorProto, String])
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls ($javaFqn), values: ${
    values.map { case (v, s) => s"${v.name} -> object $s" }.mkString(", ")
  }"
}

case class FieldSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: FieldDescriptorProto,
  defn: String)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> member $defn"

}

