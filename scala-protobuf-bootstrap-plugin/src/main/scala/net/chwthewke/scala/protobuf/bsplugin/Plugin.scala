package net.chwthewke.scala.protobuf.bsplugin

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import net.chwthewke.scala.protobuf.bsplugin.gen.FileDescriptorProcess
import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTableProcess
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scala.collection.JavaConverters.seqAsJavaListConverter
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait Plugin {

  def process: Process[CodeGeneratorResponse] = {

    for {
      req <- Process.ask :+> "Scala-Protobuf plugin started"
      protoSymbolTable <- ProtoSymbolTableProcess()
      files <- req.protoFileList.map(FileDescriptorProcess(protoSymbolTable, _)).sequence
    } yield CodeGeneratorResponse.newBuilder
      .addAllFile(files.asJava)
      .build

  }

}

object Plugin extends Plugin
