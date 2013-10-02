package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehuggerDSL._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import net.chwthewke.scala.protobuf.symbols.ProtoSymbolTable
import net.chwthewke.scala.protobuf.symbols.MessageSymbol

trait DescriptorProcess {

  import MessageContainer._
  import PluginOps._

  def symbolTable: ProtoSymbolTable

  def self: DescriptorProto

  lazy val symbol: MessageSymbol = symbolTable.symbols.collectFirst {
    case ms @ MessageSymbol(_, _, _, descriptor, _, _) if descriptor == self => ms
  }.get

  def apply: Process[Vector[Tree]] = {

    for {
      nested <- MessageContainerProcess(self, symbolTable)
      enums <- self.enumTypeList.map(EnumDescriptorProcess(_, symbolTable)).sequence
    } yield Vector[Tree](
      CASECLASSDEF(symbol.cls),
      OBJECTDEF(symbol.obj) := BLOCK(nested ++ enums.flatten)
    )

  }
}

object DescriptorProcess {
  def apply(desc: DescriptorProto, sym: ProtoSymbolTable) =
    new DescriptorProcess {
      def self = desc
      def symbolTable = sym
    }.apply

}
