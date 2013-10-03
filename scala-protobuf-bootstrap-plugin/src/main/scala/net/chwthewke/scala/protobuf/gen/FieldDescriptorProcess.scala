package net.chwthewke.scala.protobuf.gen

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import treehugger.forest._
import treehugger.forest.{ Type => SType }
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._
import net.chwthewke.scala.protobuf.Process
import net.chwthewke.scala.protobuf.syntax._
import net.chwthewke.scala.protobuf.symbols.{ MessageSymbol, EnumSymbol, ByteStringClass, ProtoSymbolTable }
import net.chwthewke.scala.protobuf.symbols.FieldSymbol

trait FieldDescriptorProcess {

  def self: FieldDescriptorProto

  def symbolTable: ProtoSymbolTable

  def apply: Process[ValDef] = Process {
    val symbol: FieldSymbol = symbolTable.field(self).get
    VAL(symbol.defn, symbol.fieldType)
  }

}

object FieldDescriptorProcess {
  def apply(field: FieldDescriptorProto, sym: ProtoSymbolTable): Process[ValDef] =
    new FieldDescriptorProcess {
      def self = field
      def symbolTable = sym
    }.apply
}
