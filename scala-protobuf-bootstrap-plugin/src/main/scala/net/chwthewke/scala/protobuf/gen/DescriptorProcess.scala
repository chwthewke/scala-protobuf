package net.chwthewke.scala.protobuf.gen

import com.google.protobuf.DescriptorProtos._
import net.chwthewke.scala.protobuf._
import net.chwthewke.scala.protobuf.symbols.MessageSymbol
import net.chwthewke.scala.protobuf.symbols.ProtoSymbolTable
import net.chwthewke.scala.protobuf.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._
import net.chwthewke.scala.protobuf.symbols.FieldSymbol

trait DescriptorProcess {

  def symbolTable: ProtoSymbolTable

  def self: DescriptorProto

  lazy val symbol: MessageSymbol = symbolTable.message(self).get

  def apply: Process[Vector[Tree]] = {

    for {
      nested <- MessageContainerProcess(self, symbolTable)
      enums <- self.enumTypeList.map(EnumDescriptorProcess(_, symbolTable)).sequence
      fields <- self.fieldList.map(classVal).sequence
    } yield Vector[Tree](
      CASECLASSDEF(symbol.cls) withParams (fields),
      objectDef(nested ++ enums.flatten)
    )

  }

  private def objectDef(content: Vector[Tree]): Tree = {
    val od = OBJECTDEF(symbol.obj)
    if (content.isEmpty) od
    else od := BLOCK(content)
  }

  private def classVal(field: FieldDescriptorProto): Process[ValDef] = process {
    val symbol: FieldSymbol = symbolTable.field(field).get
    VAL(symbol.defn, symbol.fieldType)
  }

}

object DescriptorProcess {
  def apply(desc: DescriptorProto, sym: ProtoSymbolTable) =
    new DescriptorProcess {
      def self = desc
      def symbolTable = sym
    }.apply

}
