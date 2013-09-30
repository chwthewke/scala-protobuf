package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File
import net.chwthewke.scala.protobuf.symbols.SymbolTable
import net.chwthewke.scala.protobuf.symbols.FileSymbols
import net.chwthewke.scala.protobuf.symbols.SymbolTable
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait FileDescriptorProcess {

  import PluginOps._
  import MessageContainer._
  import treehugger.forest._
  import treehugger.forest.definitions._
  import treehuggerDSL._

  def file: FileDescriptorProto

  def symbolTable: SymbolTable

  def moduleName = moduleSymbols.obj.nameString

  def targetFile: String = (file.javaPackage.split('.') :+ s"$moduleName.scala").mkString("/")

  def fileDef: Process[PackageDef] = {
    for {
      messageDefs <- MessageContainerProcess(file, symbolTable)
    } yield BLOCK(
      OBJECTDEF(moduleSymbols.obj) := BLOCK(
        messageDefs
      )
    ) inPackage (moduleSymbols.pkg)
  }

  def responseFile: Process[CodeGeneratorResponse.File] = for {
    fileDef <- this.fileDef
  } yield File.newBuilder
    .setName(targetFile)
    .setContent(treeToString(fileDef))
    .build

  private def moduleSymbols: FileSymbols = {
    symbolTable.files(file)
  }

}

object FileDescriptorProcess {
  def apply(symbolTable_ : SymbolTable, file_ : FileDescriptorProto): Process[CodeGeneratorResponse.File] =
    new FileDescriptorProcess {
      override val file = file_
      override val symbolTable = symbolTable_
    }.responseFile
}

