package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import scala.collection.JavaConverters.seqAsJavaListConverter
import scalaz.std.vector._
import scalaz.syntax.traverse._

import PluginOps._

trait Plugin {

  def process: Process[CodeGeneratorResponse] = {

    for {
      req <- Process.ask :+> "Scala-Protobuf plugin started"
      files <- req.protoFileList.map(FileDescriptorProcess(_)).sequence
    } yield CodeGeneratorResponse.newBuilder
      .addAllFile(files.asJava)
      .build

  }

}

object Plugin extends Plugin
