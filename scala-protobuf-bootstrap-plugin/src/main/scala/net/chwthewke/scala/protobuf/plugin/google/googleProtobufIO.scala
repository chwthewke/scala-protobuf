package net.chwthewke.scala.protobuf.plugin.google

import java.io.InputStream
import scala.util.Try
import scala.language.postfixOps
import net.chwthewke.scala.protobuf.plugin.interface

object googleProtobufIO extends interface.IO {
  import com.google.protobuf.compiler.PluginProtos.{ CodeGeneratorRequest => PbCodeGeneratorRequest }
  import com.google.protobuf.compiler.PluginProtos.{ CodeGeneratorResponse => PbCodeGeneratorResponse }
  import com.google.protobuf.{ DescriptorProtos => pb }
  import net.chwthewke.scala.protobuf.plugin.interface._
  import net.chwthewke.scala.protobuf.plugin.interface.field.{ Label, Type }
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
      sourceCodeInfo = maybe(fdp)(_.hasSourceCodeInfo)(_.getSourceCodeInfo) map googleProtobufIO.sourceCodeInfo)

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

  private implicit def codeGeneratorResponseToGoogleProtobuf(resp: CodeGeneratorResponse): PbCodeGeneratorResponse = {
    import scala.collection.JavaConverters.seqAsJavaListConverter
    import PbCodeGeneratorResponse.{ File => PbFile }
    
    PbCodeGeneratorResponse.newBuilder
      .addAllFile(resp.files map (f => PbFile.newBuilder.setName(f.path).setContent(f.contents).build) asJava)
      .build
  }
  
  def parseFrom(in: InputStream): Try[CodeGeneratorRequest] = Try { PbCodeGeneratorRequest.parseFrom(in) }

  def write(defs: CodeGeneratorResponse): Unit = defs.writeTo(System.out)


}
