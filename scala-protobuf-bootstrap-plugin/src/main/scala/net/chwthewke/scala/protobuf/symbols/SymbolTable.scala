package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{DescriptorProto, FileDescriptorProto}
import treehugger.forest._


case class SymbolTable(files: Map[FileDescriptorProto, ModuleClassSymbol], messages: Map[DescriptorProto, MessageSymbols]) {
  def ++(other: SymbolTable) = SymbolTable(files ++ other.files, messages ++ other.messages)
}

case class MessageSymbols(cls: ClassSymbol, obj: ModuleClassSymbol)


