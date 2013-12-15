package net.chwthewke.scala.protobuf.bsplugin.templates

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{ Type => FType }
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.symbols._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait TemplatesProcess {

  def symbolTable: ProtoSymbolTable
  def filesToGenerate: Vector[FileDescriptorProto]

  def apply: Process[Vector[ProtoDef]] =
    (filesToGenerate map (protoDef)).sequence

  def protoDef(fileDescriptor: FileDescriptorProto): Process[ProtoDef] = for {
    enums <- (fileDescriptor.enumTypeList map enumDef).sequence
    messages <- (fileDescriptor.messageTypeList map messageDef).sequence
    file: FileSymbol = symbolTable.file(fileDescriptor).get
  } yield ProtoDef(file.pkg.toString, file.obj.toString, enums ++ messages)

  def enumDef(desc: EnumDescriptorProto): Process[EnumDef] = process {
    val enum: EnumSymbol = symbolTable.enum(desc).get
    EnumDef(enum.cls, (enum.values map (enumValueDef _).tupled).toSeq)
  }

  def enumValueDef(desc: EnumValueDescriptorProto, name: String): EnumValueDef =
    EnumValueDef(name, desc.number)

  def messageDef(desc: DescriptorProto): Process[MessageDef] = for {
    enums <- (desc.enumTypeList map enumDef).sequence
    nested <- (desc.nestedTypeList map messageDef).sequence

    fields <- fieldDefs(desc)

    message: MessageSymbol = symbolTable.message(desc).get
  } yield MessageDef(message.cls, fields, enums ++ nested)

  def fieldDefs(desc: DescriptorProto): Process[FieldDefs] = for {
    fields <- (desc.fieldList map fieldDef).sequence
  } yield FieldDefs(fields)

  def fieldDef(desc: FieldDescriptorProto): Process[FieldDef] = process {
    val field: FieldSymbol = symbolTable.field(desc).get

    val (ctor, compMult) = desc.label match {
      case LABEL_OPTIONAL                => ("Optional", Some("Option"))
      case LABEL_REQUIRED                => ("Required", None)
      case LABEL_REPEATED if desc.packed => ("Packed", Some("Vector"))
      case _                             => ("Repeated", Some("Vector"))
    }

    val (compType, fieldType) = symbolTable.typeRef(field) match {
      case PrimitiveRef(p) => (p, primFieldType(desc.typ))
      case EnumRef(e)      => (e, s"net.chwthewke.scala.protobuf.FieldType.Enum($e)")
      case MessageRef(m)   => (m, s"net.chwthewke.scala.protobuf.FieldType.MessageField($m)")
    }

    FieldDef(field.defn, desc.number, ctor,
      compType, compMult, fieldType)
  }

  def primFieldType(typ: FType): String = "net.chwthewke.scala.protobuf.FieldType." + (typ match {
    case TYPE_BOOL     => "Bool"
    case TYPE_BYTES    => "Bytes"
    case TYPE_DOUBLE   => "Double"
    case TYPE_FIXED32  => "Fixed32"
    case TYPE_FIXED64  => "Fixed64"
    case TYPE_FLOAT    => "Float"
    case TYPE_INT32    => "Int32"
    case TYPE_INT64    => "Int64"
    case TYPE_SFIXED32 => "SFixed32"
    case TYPE_SFIXED64 => "SFixed64"
    case TYPE_SINT32   => "SInt32"
    case TYPE_SINT64   => "SInt64"
    case TYPE_STRING   => "String"
    case TYPE_UINT32   => "UInt32"
    case TYPE_UINT64   => "UInt64"
    case _             => throw new IllegalArgumentException
  })
}

object TemplatesProcess {
  def apply(symTable: ProtoSymbolTable, files: Vector[FileDescriptorProto]): TemplatesProcess =
    new TemplatesProcess {
      override def symbolTable = symTable
      override def filesToGenerate = files
    }
}
