package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.DescriptorProtos._
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.language.implicitConversions
import scala.language.reflectiveCalls

trait PluginOps {

  implicit private def ToVector[A](it: java.util.List[A]) = new {
    def toVector = it.asScala.toVector
  }

  // TODO eager conversion ?

  implicit class CodeGeneratorRequestOps(val self: CodeGeneratorRequest) {
    def protoFileList = self.getProtoFileList.toVector
  }

  implicit class FileDescriptorProtoOps(self: FileDescriptorProto) {
    def name = self.getName
    def pkg = self.getPackage
    def options = self.getOptions
    def messageTypeList = self.getMessageTypeList.toVector
    def dependencyList = self.getDependencyList.toVector

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

  implicit class EnumValueDescriptorProtoOps(self : EnumValueDescriptorProto) {
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
}

object PluginOps extends PluginOps
