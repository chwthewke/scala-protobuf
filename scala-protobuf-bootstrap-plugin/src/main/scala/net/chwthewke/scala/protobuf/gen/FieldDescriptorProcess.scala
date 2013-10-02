package net.chwthewke.scala.protobuf.gen

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import treehugger.forest._
import treehugger.forest.definitions._
import net.chwthewke.scala.protobuf.Process
import net.chwthewke.scala.protobuf.syntax._
import net.chwthewke.scala.protobuf.symbols.ByteStringClass
import net.chwthewke.scala.protobuf.symbols.ProtoSymbolTable

trait FieldDescriptorProcess {

  def self: FieldDescriptorProto

  def symbolTable: ProtoSymbolTable

  def apply: Process[Tree]

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
    case (TYPE_GROUP, _) | (TYPE_MESSAGE, _) =>
      self.typeName match {case Some(typeName) => message(typeName)}
    case (TYPE_STRING, _) => StringClass
  }

  private def enum(typeName: String): Symbol

  private def message(typeName: String): Symbol

}