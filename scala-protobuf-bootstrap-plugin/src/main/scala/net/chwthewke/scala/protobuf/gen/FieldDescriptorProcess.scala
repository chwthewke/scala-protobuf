package net.chwthewke.scala.protobuf.gen

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import treehugger.forest._
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._
import net.chwthewke.scala.protobuf.Process
import net.chwthewke.scala.protobuf.syntax._
import net.chwthewke.scala.protobuf.symbols.{ MessageSymbol, EnumSymbol, ByteStringClass, ProtoSymbolTable }
import net.chwthewke.scala.protobuf.symbols.FieldSymbol

trait FieldDescriptorProcess {

  def self: FieldDescriptorProto

  lazy val symbol: FieldSymbol = symbolTable.field(self).get

  def file: FileDescriptorProto = symbol.file
  def fqn: String = symbol.fqn

  def symbolTable: ProtoSymbolTable

  def apply: Process[ValDef] = Process {
    val typ = typeSymbol.lift(self.typ -> self.typeName).get

    VAL(symbol.defn, typ)
  }

  private def typeSymbol: PartialFunction[(Type, Option[String]), Symbol] = {
    case (TYPE_BOOL, _) => BooleanClass
    case (TYPE_BYTES, _) => ByteStringClass
    case (TYPE_DOUBLE, _) => DoubleClass
    case (TYPE_ENUM, Some(typeName)) => enum(typeName)
    case (TYPE_FIXED32, _) | (TYPE_INT32, _) |
      (TYPE_SFIXED32, _) | (TYPE_SINT32, _) |
      (TYPE_UINT32, _) => IntClass
    case (TYPE_FIXED64, _) | (TYPE_INT64, _) |
      (TYPE_SFIXED64, _) | (TYPE_SINT64, _) |
      (TYPE_UINT64, _) => LongClass
    case (TYPE_FLOAT, _) => FloatClass
    case (TYPE_GROUP, Some(typeName)) => message(typeName)
    case (TYPE_MESSAGE, Some(typeName)) => message(typeName)
    case (TYPE_STRING, _) => StringClass
  }

  def enum(typeName: String): Symbol = symbolTable.findByName(typeName, fqn, file).collect {
    case EnumSymbol(_, _, _, _, cls, _) => cls
  }.get

  def message(typeName: String): Symbol = symbolTable.findByName(typeName, fqn, file).collect {
    case MessageSymbol(_, _, _, _, cls, _) => cls
  }.get

}

object FieldDescriptorProcess {
  def apply(field: FieldDescriptorProto, sym: ProtoSymbolTable): Process[ValDef] =
    new FieldDescriptorProcess {
      def self = field
      def symbolTable = sym
    }.apply
}
