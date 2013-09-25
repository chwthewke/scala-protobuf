package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import scala.collection.JavaConverters.seqAsJavaListConverter


import PluginOps._

trait Plugin {

  def process(request: CodeGeneratorRequest): CodeGeneratorResponse = {

    val codeFiles = request.protoFileList.map {
      f =>
        val fp = new FileProcessor {def in: FileDescriptorProto = f}
        CodeGeneratorResponse.File.newBuilder
          .setName(fp.targetFile)
          .setContent("")
          .build
    }

    CodeGeneratorResponse.newBuilder
      .addAllFile(codeFiles.asJava)
      .build
  }

}

trait FileProcessor {


  def in: FileDescriptorProto

  def targetFile: String = (pkg.split('.') + className).mkString("/")

  def pkg = Option(in.options.javaPackage).getOrElse(in.pkg)

  def className = Option(in.options.javaOuterClassName).getOrElse(in.name.capitalize)

}
