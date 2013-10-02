package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.SourceCodeInfo.Location
import net.chwthewke.scala.protobuf.PluginOps._
import treehugger.forest._

case class ProtoSymbolTable(symbols: Vector[ProtoSymbol])

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
  cls: ClassSymbol,
  obj: ModuleClassSymbol)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls, object $obj"
}

case class FileSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: FileDescriptorProto,
  obj: ModuleClassSymbol,
  pkg: ModuleClassSymbol)
  extends ProtoSymbol {

  def mkString = s"${file.name} -> object $obj"
}

case class EnumSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: EnumDescriptorProto,
  cls: ClassSymbol,
  values: Map[EnumValueDescriptorProto, ModuleClassSymbol])
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> class $cls, values: ${
    values.map { case (v, s) => s"${v.name} -> object $s" }.mkString(", ")
  }"
}

