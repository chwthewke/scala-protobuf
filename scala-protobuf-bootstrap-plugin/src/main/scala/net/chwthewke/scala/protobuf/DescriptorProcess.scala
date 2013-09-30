package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.symbols.SymbolTable
import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehuggerDSL._

trait DescriptorProcess {
  def apply(symbolTable: SymbolTable, file: FileDescriptorProto, message: DescriptorProto): Process[Vector[Tree]] = {
    Process {
      val sym = symbolTable.messages(message)
      Vector[Tree](
        OBJECTDEF(sym.obj),
        CASECLASSDEF(sym.cls), PAREN()) // TODO no case class if no fields ?
    }
  }
}

object DescriptorProcess extends DescriptorProcess
