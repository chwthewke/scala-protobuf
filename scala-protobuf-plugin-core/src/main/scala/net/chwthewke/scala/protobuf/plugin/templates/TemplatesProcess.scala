package net.chwthewke.scala.protobuf.plugin.templates

import net.chwthewke.scala.protobuf.plugin._
import net.chwthewke.scala.protobuf.plugin.interface._
import net.chwthewke.scala.protobuf.plugin.interface.field._
import net.chwthewke.scala.protobuf.plugin.symbols._
import net.chwthewke.scala.protobuf.plugin.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait TemplatesProcess {

  def symbolTable: ProtoSymbolTable
  def filesToGenerate: Vector[FileDescriptor]

  def apply: Process[Vector[ProtoDef]] =
    (filesToGenerate map (protoDef)).sequence

  def protoDef(fileDescriptor: FileDescriptor): Process[ProtoDef] = for {
    enums <- (fileDescriptor.enumTypes map enumDef).sequence
    messages <- (fileDescriptor.messageTypes map messageDef).sequence
    file: FileSymbol = symbolTable.file(fileDescriptor).get
  } yield ProtoDef(file.pkg.toString, file.obj.toString, enums ++ messages)

  def enumDef(desc: EnumDescriptor): Process[EnumDef] = process {
    val enum: EnumSymbol = symbolTable.enum(desc).get
    EnumDef(enum.cls, (enum.values map (enumValueDef _).tupled).toSeq)
  }

  def enumValueDef(desc: EnumValueDescriptor, name: String): EnumValueDef =
    EnumValueDef(name, desc.number)

  def messageDef(desc: Descriptor): Process[MessageDef] = for {
    enums <- (desc.enumTypes map enumDef).sequence
    nested <- (desc.nestedTypes map messageDef).sequence

    fields <- fieldDefs(desc)

    message: MessageSymbol = symbolTable.message(desc).get
  } yield MessageDef(message.cls, fields, enums ++ nested)

  def fieldDefs(desc: Descriptor): Process[FieldDefs] = for {
    fields <- (desc.fields map fieldDef).sequence
  } yield FieldDefs(fields)

  def fieldDef(desc: FieldDescriptor): Process[FieldDef] = process {
    val field: FieldSymbol = symbolTable.field(desc).get

    val (ctor, compMult) = desc.label match {
      case LABEL_OPTIONAL                => ("Optional", Some("Option"))
      case LABEL_REQUIRED                => ("Required", None)
      case LABEL_REPEATED if desc.packed => ("Packed", Some("Vector"))
      case _                             => ("Repeated", Some("Vector"))
    }

    val typeRef: ProtoRef = symbolTable.typeRef(field)

    val (compType, fieldType) = typeRef match {
      case PrimitiveRef(p) => (p, primFieldType(desc.typ))
      case EnumRef(e)      => (e, s"net.chwthewke.scala.protobuf.FieldType.Enum($e)")
      case MessageRef(m)   => (m, s"net.chwthewke.scala.protobuf.FieldType.MessageField($m)")
    }

    def literalDefault: Option[String] = desc.defaultValue match {
      case None => None
      case Some(d) => typeRef match {
        case BoolRef    => Some(d)
        case EnumRef(e) => Some(s"$e.$d")
        case _          => None
      }
    }

    val fieldDefault: FieldDefaultDef = desc.label match {
      case LABEL_REPEATED => EmptyVector
      case LABEL_OPTIONAL => literalDefault map SomeLiteral getOrElse NoneOption
      case LABEL_REQUIRED => literalDefault map Literal getOrElse NoDefault
    }

    FieldDef(field.defn, desc.number, ctor,
      compType, compMult, fieldDefault, fieldType)
  }

  def primFieldType(typ: Type): String = "net.chwthewke.scala.protobuf.FieldType." + (typ match {
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
  def apply(symTable: ProtoSymbolTable, files: Vector[FileDescriptor]): TemplatesProcess =
    new TemplatesProcess {
      override def symbolTable = symTable
      override def filesToGenerate = files
    }
}
