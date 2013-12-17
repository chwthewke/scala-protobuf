package net.chwthewke.scala.protobuf.plugin

import java.io.InputStream
import scala.util.Try
import scala.language.postfixOps

object interface {

  case class CodeGeneratorRequest(
    protoFiles: Vector[FileDescriptor],
    filesToGenerate: Vector[String])

  case class FileDescriptor(
    name: String,
    pkg: String,
    options: FileOptions,
    messageTypes: Vector[Descriptor],
    enumTypes: Vector[EnumDescriptor],
    dependencies: Vector[String],
    sourceCodeInfo: Option[SourceCodeInfo]) {

    def javaPackage = options.javaPackage.getOrElse(pkg)
    def javaOuterClassName = options.javaOuterClassName.getOrElse(name.stripSuffix(".proto").capitalize)
  }

  case class FileOptions(
    javaPackage: Option[String],
    javaOuterClassName: Option[String])

  case class Descriptor(
    name: String,
    fields: Vector[FieldDescriptor],
    enumTypes: Vector[EnumDescriptor],
    nestedTypes: Vector[Descriptor])

  case class EnumDescriptor(
    name: String,
    values: Vector[EnumValueDescriptor])

  case class EnumValueDescriptor(
    name: String,
    number: Int)

  object field {

    sealed trait Label
    case object LABEL_REQUIRED extends Label
    case object LABEL_OPTIONAL extends Label
    case object LABEL_REPEATED extends Label

    sealed trait Type
    case object TYPE_DOUBLE extends Type
    case object TYPE_FLOAT extends Type
    case object TYPE_INT32 extends Type
    case object TYPE_UINT32 extends Type
    case object TYPE_SINT32 extends Type
    case object TYPE_FIXED32 extends Type
    case object TYPE_SFIXED32 extends Type
    case object TYPE_INT64 extends Type
    case object TYPE_UINT64 extends Type
    case object TYPE_SINT64 extends Type
    case object TYPE_FIXED64 extends Type
    case object TYPE_SFIXED64 extends Type
    case object TYPE_BOOL extends Type
    case object TYPE_STRING extends Type
    case object TYPE_BYTES extends Type
    case object TYPE_MESSAGE extends Type
    case object TYPE_GROUP extends Type
    case object TYPE_ENUM extends Type
  }

  case class FieldDescriptor(
    name: String,
    number: Int,
    label: field.Label,
    packed: Boolean,
    typ: field.Type,
    typeName: Option[String],
    defaultValue: Option[String])

  case class SourceCodeInfo(
    locations: Vector[Location]) {

    def findLocation(path: Vector[Int]) = locations.find(path == _.path)
  }

  case class Location(
    path: Vector[Int],
    span: Vector[Int]) {

    def startLine: Int = span(0)
    def endLine: Int = span(if (isShort) 0 else 2)
    def startCol: Int = span(1)
    def endCol: Int = span(if (isShort) 2 else 3)

    def isShort: Boolean = span.size == 3

    def mkString = {
      if (isShort) s"${span(0)}, ${span(1)}-${span(2)}"
      else s"(${span(0)},${span(1)})-(${span(2)},${span(3)})"
    }
  }

  case class CodeGeneratorResponse(files: Seq[File], error: Option[String])

  case class File(path: String, contents: String)

  trait IO {
    def parseFrom(in: InputStream): Try[CodeGeneratorRequest]
    def write(defs: CodeGeneratorResponse): Unit
  }

}
