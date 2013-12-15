package net.chwthewke.scala.protobuf.bsplugin

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTableProcess
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import net.chwthewke.scala.protobuf.bsplugin.templates.ProtoDef
import net.chwthewke.scala.protobuf.bsplugin.templates.TemplatesProcess
import scala.collection.JavaConverters.seqAsJavaListConverter
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait Plugin {

  def process: Process[CodeGeneratorResponse] = {

    for {
      req <- Process.ask :+> "Scala-Protobuf plugin started"
      protoSymbolTable <- ProtoSymbolTableProcess()
      filesToGenerate <- filesToGenerate
      protoDefs <- TemplatesProcess(protoSymbolTable, filesToGenerate).apply
      files = protoDefs map toFile
    } yield CodeGeneratorResponse.newBuilder
      .addAllFile(files.asJava)
      .build
  }

  def toFile(protoDef: ProtoDef): CodeGeneratorResponse.File =
    CodeGeneratorResponse.File.newBuilder
      .setName(protoDef.file)
      .setContent(protoDef.all)
      .build

  def filesToGenerate: Process[Vector[FileDescriptorProto]] = {
    val result: Process[Vector[FileDescriptorProto]] = for {
      req <- Process.ask
    } yield req.protoFileList.filter((fdp: FileDescriptorProto) => req.fileToGenerateList.contains(fdp.name))

    result :+>> (
      (l: Vector[FileDescriptorProto]) => s"Generating source for ${l.map(_.name).mkString(", ")}")
  }

}

object Plugin extends Plugin
