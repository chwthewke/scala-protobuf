package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import scala.collection.JavaConverters.seqAsJavaListConverter
import scalaz.std.vector._
import scalaz.syntax.traverse._

import PluginOps._
import net.chwthewke.scala.protobuf.symbols.SymbolTableProcess

trait Plugin {

  def process: Process[CodeGeneratorResponse] = {

    for {
      req <- Process.ask :+> "Scala-Protobuf plugin started"
      symbolTable <- SymbolTableProcess()
      files <- req.protoFileList.map(FileDescriptorProcess(symbolTable, _)).sequence
    } yield CodeGeneratorResponse.newBuilder
      .addAllFile(files.asJava)
      .build

  }

}

object Plugin extends Plugin
