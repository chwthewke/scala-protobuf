package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{ Type => FType }
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import treehugger._
import treehugger.forest._
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._

private[symbols] object ComputeFieldTypes {

  def apply(symbolTable: ProtoSymbolTable): ProtoSymbolTable = ProtoSymbolTable(
    symbolTable.symbols.map {
      case fs: RawFieldSymbol => computeFieldType(symbolTable, fs)
      case s => s
    })

  def computeFieldType(symTbl: ProtoSymbolTable, f: RawFieldSymbol): FieldSymbol = {

    new ComputeFieldTypes {
      def symbolTable = symTbl
      def fieldSymbol = f
    }.apply()
  }

}

trait ComputeFieldTypes {
  def symbolTable: ProtoSymbolTable

  def fieldSymbol: RawFieldSymbol

  def field: FieldDescriptorProto = fieldSymbol.descriptor

  def fqn: String = fieldSymbol.fqn

  def file: FileDescriptorProto = fieldSymbol.file

  def apply(): FieldSymbol = {
    val (ref, typ) = typeSymbol.lift(fieldSymbol.descriptor.typ -> fieldSymbol.descriptor.typeName).get

    val modTyp: Type = (typ, fieldSymbol.descriptor.label) match {
      case (cls: ClassSymbol, LABEL_OPTIONAL) => optionType(cls)
      case (cls: ClassSymbol, LABEL_REPEATED) => appliedType(VectorClass.typeConstructor, List[Type](cls))
      case _ => typ
    }

    new FieldSymbol(fieldSymbol, ref, typ, modTyp)
  }

  private def typeSymbol: PartialFunction[(FType, Option[String]), (ProtoRef, Symbol)] =
    (simpleTypeSymbol andThen (s => (NoProtoRef, s))) orElse referenceTypeSymbol

  private def referenceTypeSymbol: PartialFunction[(FType, Option[String]), (ProtoRef, Symbol)] = {
    case (TYPE_ENUM, Some(typeName)) => enum(typeName)
    case (TYPE_GROUP, Some(typeName)) => message(typeName)
    case (TYPE_MESSAGE, Some(typeName)) => message(typeName)
  }

  private def simpleTypeSymbol: PartialFunction[(FType, Option[String]), Symbol] = {
    case (TYPE_BOOL, _) => BooleanClass
    case (TYPE_BYTES, _) => ByteStringClass
    case (TYPE_DOUBLE, _) => DoubleClass
    case (TYPE_FIXED32, _) | (TYPE_INT32, _) |
      (TYPE_SFIXED32, _) | (TYPE_SINT32, _) |
      (TYPE_UINT32, _) => IntClass
    case (TYPE_FIXED64, _) | (TYPE_INT64, _) |
      (TYPE_SFIXED64, _) | (TYPE_SINT64, _) |
      (TYPE_UINT64, _) => LongClass
    case (TYPE_FLOAT, _) => FloatClass
    case (TYPE_STRING, _) => StringClass
  }

  def enum(typeName: String): (ProtoRef, Symbol) = symbolTable.findByName(typeName, fqn, file).collect {
    case EnumSymbol(_, _, _, desc, cls, _) => (EnumRef(desc), cls)
  }.get

  def message(typeName: String): (ProtoRef, Symbol) = symbolTable.findByName(typeName, fqn, file).collect {
    case MessageSymbol(_, _, _, desc, cls, _) => (MessageRef(desc), cls)
  }.get

}