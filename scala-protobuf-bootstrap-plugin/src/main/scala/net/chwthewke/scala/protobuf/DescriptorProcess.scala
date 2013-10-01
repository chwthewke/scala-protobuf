package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.symbols.SymbolTable
import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehuggerDSL._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait DescriptorProcess {

  import MessageContainer._
  import PluginOps._

  def symbolTable: SymbolTable

  def descriptor: DescriptorProto

  def apply: Process[Vector[Tree]] = {

    val sym = symbolTable.messages.symbols(descriptor)
    for {
      nested <- MessageContainerProcess(descriptor, symbolTable)
      enums <- descriptor.enumTypeList.map(EnumDescriptorProcess(_, symbolTable)).sequence
    } yield Vector[Tree](
      CASECLASSDEF(sym.cls),
      OBJECTDEF(sym.obj) := BLOCK(nested ++ enums.flatten)
    )

  }
}

object DescriptorProcess {
  def apply(desc: DescriptorProto, sym: SymbolTable) =
    new DescriptorProcess {
      def descriptor = desc
      def symbolTable = sym
    }.apply

}
