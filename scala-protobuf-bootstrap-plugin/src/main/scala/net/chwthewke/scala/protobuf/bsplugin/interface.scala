package net.chwthewke.scala.protobuf.bsplugin

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

  object google extends IO {
    import com.google.protobuf.compiler.PluginProtos.{ CodeGeneratorRequest => PbCodeGeneratorRequest }
    import com.google.protobuf.compiler.PluginProtos.{ CodeGeneratorResponse => PbCodeGeneratorResponse }
    import com.google.protobuf.{ DescriptorProtos => pb }
    import net.chwthewke.scala.protobuf.bsplugin.interface.field.{ Label, Type }
    import scala.collection.JavaConverters.iterableAsScalaIterableConverter
    import scala.language.implicitConversions

    private implicit class toVector[A](i: java.lang.Iterable[A]) {
      def toVector: Vector[A] = i.asScala.toVector
    }

    private def maybe[A, B](a: A)(has: A => Boolean)(get: A => B): Option[B] =
      if (has(a)) Some(get(a)) else None

    implicit def googleProtobufToCodeGeneratorRequest(in: PbCodeGeneratorRequest): CodeGeneratorRequest = {
      CodeGeneratorRequest(
        protoFiles = in.getProtoFileList.toVector map fileDescriptor,
        filesToGenerate = in.getFileToGenerateList.toVector)
    }

    private def fileDescriptor(fdp: pb.FileDescriptorProto): FileDescriptor =
      FileDescriptor(
        name = fdp.getName,
        pkg = fdp.getPackage,
        options = fileOptions(fdp.getOptions),
        dependencies = fdp.getDependencyList.toVector,
        enumTypes = fdp.getEnumTypeList.toVector map enumDescriptor,
        messageTypes = fdp.getMessageTypeList.toVector map messageDescriptor,
        sourceCodeInfo = maybe(fdp)(_.hasSourceCodeInfo)(_.getSourceCodeInfo) map google.sourceCodeInfo)

    private def sourceCodeInfo(sci: pb.SourceCodeInfo): SourceCodeInfo =
      SourceCodeInfo(
        locations = sci.getLocationList.toVector map location)

    private def location(loc: pb.SourceCodeInfo.Location): Location =
      Location(
        span = loc.getSpanList.toVector map Integer2int,
        path = loc.getPathList.toVector map Integer2int)

    private def fileOptions(fo: pb.FileOptions): FileOptions =
      FileOptions(
        javaOuterClassName = maybe(fo)(_.hasJavaOuterClassname)(_.getJavaOuterClassname),
        javaPackage = maybe(fo)(_.hasJavaPackage)(_.getJavaPackage))

    private def enumDescriptor(edp: pb.EnumDescriptorProto): EnumDescriptor =
      EnumDescriptor(
        name = edp.getName,
        values = edp.getValueList.toVector map enumValueDescriptor)

    private def enumValueDescriptor(evdp: pb.EnumValueDescriptorProto): EnumValueDescriptor =
      EnumValueDescriptor(
        name = evdp.getName,
        number = evdp.getNumber)

    private def messageDescriptor(dp: pb.DescriptorProto): Descriptor =
      Descriptor(
        name = dp.getName,
        fields = dp.getFieldList.toVector map fieldDescriptor,
        enumTypes = dp.getEnumTypeList.toVector map enumDescriptor,
        nestedTypes = dp.getNestedTypeList.toVector map messageDescriptor)

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
        name = fdp.getName,
        number = fdp.getNumber,
        label = fieldLabel(fdp.getLabel),
        typ = fieldType(fdp.getType),
        typeName = maybe(fdp)(_.hasTypeName)(_.getTypeName),
        packed = fdp.getOptions.getPacked,
        defaultValue = maybe(fdp)(_.hasDefaultValue)(_.getDefaultValue))

    def parseFrom(in: InputStream): Try[CodeGeneratorRequest] = Try { PbCodeGeneratorRequest.parseFrom(in) }

    def write(defs: CodeGeneratorResponse): Unit =
      {
        import PbCodeGeneratorResponse.{File => PbFile}
        import scala.collection.JavaConverters.seqAsJavaListConverter

        val files = defs.files map (f => PbFile.newBuilder.setName(f.path).setContent(f.contents).build) asJava

        PbCodeGeneratorResponse.newBuilder
          .addAllFile(files)
          .build
          .writeTo(System.out)
      }
  }

}
