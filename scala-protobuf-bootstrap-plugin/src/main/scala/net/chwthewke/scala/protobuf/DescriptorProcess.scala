package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.symbols.SymbolTable
import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehuggerDSL._

trait DescriptorProcess {

  import MessageContainer._

  def symbolTable: SymbolTable

  def descriptor: DescriptorProto

  def apply: Process[Vector[Tree]] = {

    val sym = symbolTable.messages(descriptor)
    for {
      nested <- MessageContainerProcess(descriptor, symbolTable)
    } yield Vector[Tree](
      CASECLASSDEF(sym.cls),
      OBJECTDEF(sym.obj) := BLOCK(nested)
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
