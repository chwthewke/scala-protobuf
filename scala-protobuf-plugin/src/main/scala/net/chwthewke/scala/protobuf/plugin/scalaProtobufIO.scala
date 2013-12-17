package net.chwthewke.scala.protobuf.plugin

import java.io.InputStream
import net.chwthewke.scala.protobuf.compiler.PluginProtos
import net.chwthewke.scala.protobuf.plugin.interface._
import net.chwthewke.scala.protobuf.plugin.interface.field.{Type, Label}
import net.chwthewke.scala.protobuf.scalautils.{ Bad, Good }
import net.chwthewke.scala.protobuf.{ DescriptorProtos => pb }
import scala.language.implicitConversions
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scalaz.syntax.id._

object scalaProtobufIO extends interface.IO {

  import PluginProtos.{ CodeGeneratorRequest => PbCodeGeneratorRequest }
  import PluginProtos.{ CodeGeneratorResponse => PbCodeGeneratorResponse }

  import net.chwthewke.scala.protobuf.ops.all._

  implicit def scalaProtobufToCodeGeneratorRequest(req: PbCodeGeneratorRequest): CodeGeneratorRequest =
    CodeGeneratorRequest(
      protoFiles = req.protoFile map fileDescriptor,
      filesToGenerate = req.fileToGenerate
    )

  private def fileDescriptor(fdp: pb.FileDescriptorProto): FileDescriptor =
    FileDescriptor(
      fdp.name.get,
      fdp.`package`.get,
      fdp.options.get |> fileOptions,
      fdp.messageType map messageType,
      fdp.enumType map enumType,
      fdp.dependency,
      fdp.sourceCodeInfo map sourceCodeInfo)

  private def fileOptions(fop: pb.FileOptions): FileOptions =
    FileOptions(fop.javaPackage, fop.javaOuterClassname)

  private def enumType(e: pb.EnumDescriptorProto): EnumDescriptor =
    EnumDescriptor(e.name.get, e.value map enumValue)

  private def enumValue(ev: pb.EnumValueDescriptorProto): EnumValueDescriptor =
    EnumValueDescriptor(ev.name.get, ev.number.get)

  private def messageType(m: pb.DescriptorProto): Descriptor =
    Descriptor(
      m.name.get,
      m.field map fieldDescriptor,
      m.enumType map enumType,
      m.nestedType map messageType)

  import pb.FieldDescriptorProto.{ Label => pbLabel }
  private def fieldLabel(l: pbLabel): Label = l match {
    case pbLabel.LABEL_REQUIRED => field.LABEL_REQUIRED
    case pbLabel.LABEL_OPTIONAL => field.LABEL_OPTIONAL
    case pbLabel.LABEL_REPEATED => field.LABEL_REPEATED
  }

  import pb.FieldDescriptorProto.{ Type => pbType }
  private def fieldType(t: pbType): Type = t match {
    case pbType.TYPE_DOUBLE   => field.TYPE_DOUBLE
    case pbType.TYPE_FLOAT    => field.TYPE_FLOAT
    case pbType.TYPE_INT32    => field.TYPE_INT32
    case pbType.TYPE_UINT32   => field.TYPE_UINT32
    case pbType.TYPE_SINT32   => field.TYPE_SINT32
    case pbType.TYPE_FIXED32  => field.TYPE_FIXED32
    case pbType.TYPE_SFIXED32 => field.TYPE_SFIXED32
    case pbType.TYPE_INT64    => field.TYPE_INT64
    case pbType.TYPE_UINT64   => field.TYPE_UINT64
    case pbType.TYPE_SINT64   => field.TYPE_SINT64
    case pbType.TYPE_FIXED64  => field.TYPE_FIXED64
    case pbType.TYPE_SFIXED64 => field.TYPE_SFIXED64
    case pbType.TYPE_BOOL     => field.TYPE_BOOL
    case pbType.TYPE_STRING   => field.TYPE_STRING
    case pbType.TYPE_BYTES    => field.TYPE_BYTES
    case pbType.TYPE_MESSAGE  => field.TYPE_MESSAGE
    case pbType.TYPE_GROUP    => field.TYPE_GROUP
    case pbType.TYPE_ENUM     => field.TYPE_ENUM
  }

  private def fieldDescriptor(fdp: pb.FieldDescriptorProto): FieldDescriptor =
    FieldDescriptor(
      fdp.name.get,
      fdp.number.get,
      fdp.label.get |> fieldLabel,
      fdp.options flatMap (_.packed) getOrElse false,
      fdp.`type`.get |> fieldType,
      fdp.typeName,
      fdp.defaultValue)

  private def sourceCodeInfo(sci: pb.SourceCodeInfo): SourceCodeInfo =
    SourceCodeInfo(sci.location map location)

  private def location(loc: pb.SourceCodeInfo.Location): Location =
    Location(loc.path, loc.span)

  implicit def codeGeneratorResponseToScalaProtobuf(req: CodeGeneratorResponse): PbCodeGeneratorResponse = ???

  def parseFrom(in: InputStream): Try[CodeGeneratorRequest] = PbCodeGeneratorRequest.parse(in) match {
    case Good(m) => Success(m)
    case Bad(e)  => Failure(new IllegalArgumentException(s"Protobuf error: $e"))
  }

  def write(defs: CodeGeneratorResponse): Unit = {
    val pbResponse: PbCodeGeneratorResponse = defs
    System.out.write(pbResponse.toByteArray)
  }

}
