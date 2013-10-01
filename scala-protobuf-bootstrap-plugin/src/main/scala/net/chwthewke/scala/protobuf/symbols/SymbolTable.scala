package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, EnumValueDescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf.PluginOps
import treehugger.forest._

// TODO enums
case class SymbolTable(files: Map[FileDescriptorProto, FileSymbols],
  messages: SymbolPaths[DescriptorProto, MessageSymbols],
  enums: SymbolPaths[EnumDescriptorProto, EnumSymbols]) {
  def ++(other: SymbolTable) = SymbolTable(
    files ++ other.files,
    messages ++ other.messages,
    enums ++ other.enums)
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

// TODO the "paths" part may be useless, check
case class SymbolPaths[P, S](symbols: Map[P, S], paths: Map[P, DescriptorPath]) {
  val protos: Map[DescriptorPath, P] = paths.map(_.swap)

  val protosByName: Map[String, (FileDescriptorProto, P)] =
    for {
      (DescriptorPath(source, names), p) <- protos
    } yield names.mkString(".") -> (source, p)

  def ++(other: SymbolPaths[P, S]) = SymbolPaths(symbols ++ other.symbols, paths ++ other.paths)
}

object DescriptorPath {
  import PluginOps._

  def apply(file: FileDescriptorProto): DescriptorPath =
    DescriptorPath(file, file.pkg.split('.').toVector)
}
// TODO for search, refactor Map[DescriptorPath, DescriptorProto] to a tree-like structure
