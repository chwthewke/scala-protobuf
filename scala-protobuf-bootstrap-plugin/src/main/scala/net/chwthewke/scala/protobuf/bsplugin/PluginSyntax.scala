package net.chwthewke.scala.protobuf.bsplugin

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.SourceCodeInfo.Location
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.language.implicitConversions
import scala.language.reflectiveCalls

trait PluginSyntax {

  implicit private def ToVector[A](it: java.util.List[A]) = new {
    def toVector = it.asScala.toVector
  }

  implicit class CodeGeneratorRequestOps(val self: CodeGeneratorRequest) {
    def protoFileList = self.getProtoFileList.toVector
    def fileToGenerateList = self.getFileToGenerateList.toVector
  }

  implicit class FileDescriptorProtoOps(self: FileDescriptorProto) {
    def name = self.getName
    def pkg = self.getPackage
    def options = self.getOptions
    def messageTypeList = self.getMessageTypeList.toVector
    def enumTypeList = self.getEnumTypeList.toVector
    def dependencyList = self.getDependencyList.toVector

    def sourceCodeInfo = Option(self.getSourceCodeInfo)

    def javaPackage = options.javaPackage.getOrElse(pkg)
    def javaOuterClassName = options.javaOuterClassName.getOrElse(name.stripSuffix(".proto").capitalize)
  }

  implicit class FileOptionsOps(self: FileOptions) {
    def javaPackage = Option(self.getJavaPackage)
    def javaOuterClassName = Option(self.getJavaOuterClassname)

  }

  implicit class DescriptorProtoOps(self: DescriptorProto) {
    def name = self.getName
    def fieldList = self.getFieldList.toVector
    def enumTypeList = self.getEnumTypeList.toVector
    def nestedTypeList = self.getNestedTypeList.toVector
  }

  implicit class EnumDescriptorProtoOps(self: EnumDescriptorProto) {
    def name = self.getName
    def valueList = self.getValueList.toVector
  }

  implicit class EnumValueDescriptorProtoOps(self: EnumValueDescriptorProto) {
    def name = self.getName
    def number = self.getNumber
  }

  implicit class FieldDescriptorProtoOps(self: FieldDescriptorProto) {
    def name = self.getName
    def label = self.getLabel
    def number = self.getNumber
    def typ = self.getType
    def typeName = Option(self.getTypeName)
    def defaultValue = Option(self.getDefaultValue)
  }

  implicit class SourceCodeInfoOps(self: SourceCodeInfo) {
    def locationList = self.getLocationList.asScala.toVector

    def findLocation(path: Vector[Int]) = locationList.find(path == _.path)
  }

  implicit class LocationOps(self: Location) {
    def startLine = self.getSpan(0)
    def endLine = self.getSpan(if (isShort) 0 else 2)
    def startCol = self.getSpan(1)
    def endCol = self.getSpan(if (isShort) 2 else 3)

    def mkString = {
      val span = self.getSpanList.asScala
      if (isShort) s"${span(0)}, ${span(1)}-${span(2)}"
      else s"(${span(0)},${span(1)})-(${span(2)},${span(3)})"
    }

    def path = self.getPathList.asScala.map(Integer2int).toVector

    private def isShort = self.getSpanCount == 3
  }

  object LocationEx {
    def unapply(loc: Location): Option[Vector[Int]] = Some(loc.getPathList.asScala.map(Integer2int).toVector)
  }
}

