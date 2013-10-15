package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.SourceCodeInfo.Location
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import treehugger.forest._

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

private[symbols] case class RawFieldSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: FieldDescriptorProto,
  defn: TermSymbol)
  extends ProtoSymbol {

  def mkString = s"$fqn in ${file.name} -> member $defn"

}

case class FieldSymbol(
  file: FileDescriptorProto,
  source: Option[Location],
  fqn: String,
  descriptor: FieldDescriptorProto,
  defn: TermSymbol,
  componentType: Type,
  fieldType: Type)
  extends ProtoSymbol {

  def this(fs: RawFieldSymbol, compType: Type, fType: Type) = this(
    fs.file, fs.source, fs.fqn, fs.descriptor, fs.defn,
    compType, fType)

  def mkString = s"$fqn in ${file.name} -> member $defn: $fieldType"

}
