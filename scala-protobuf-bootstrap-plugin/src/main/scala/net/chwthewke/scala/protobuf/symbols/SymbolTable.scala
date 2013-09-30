package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, EnumValueDescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf.PluginOps
import treehugger.forest._

// TODO enums
case class SymbolTable(files: Map[FileDescriptorProto, FileSymbols],
  messages: Map[DescriptorProto, MessageSymbols],
  enums: Map[EnumDescriptorProto, EnumSymbols],
  descriptors: Map[DescriptorPath, DescriptorProto]) {
  def ++(other: SymbolTable) = SymbolTable(
    files ++ other.files,
    messages ++ other.messages,
    enums ++ other.enums,
    descriptors ++ other.descriptors)
}

case class MessageSymbols(cls: ClassSymbol, obj: ModuleClassSymbol)

case class FileSymbols(obj: ModuleClassSymbol, pkg: ModuleClassSymbol)

case class EnumSymbols(enum: ClassSymbol, values: Map[EnumValueDescriptorProto, ModuleClassSymbol])

/**
 * name components of a descriptor's fqn
 */
case class DescriptorPath(source: FileDescriptorProto, names: Vector[String]) {
  import PluginOps._

  def +(name: String) = DescriptorPath(source, names :+ name)

  def mkString: String = s"${names.mkString(".")} in ${source.name}"
}

object DescriptorPath {
  import PluginOps._

  def apply(file: FileDescriptorProto): DescriptorPath =
    DescriptorPath(file, file.pkg.split('.').toVector)
}
// TODO for search, refactor Map[DescriptorPath, DescriptorProto] to a tree-like structure
