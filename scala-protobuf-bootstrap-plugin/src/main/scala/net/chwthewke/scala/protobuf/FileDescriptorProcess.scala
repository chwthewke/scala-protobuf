package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File
import net.chwthewke.scala.protobuf.symbols.SymbolTable
import net.chwthewke.scala.protobuf.symbols.FileSymbols
import net.chwthewke.scala.protobuf.symbols.SymbolTable

trait FileDescriptorProcess {

  import PluginOps._
  import treehugger.forest._
  import treehugger.forest.definitions._
  import treehuggerDSL._

  def file: FileDescriptorProto

  def symbolTable: SymbolTable

  def moduleName = moduleSymbols.obj.nameString

  def targetFile: String = (file.javaPackage.split('.') :+ s"$moduleName.scala").mkString("/")

  def fileDef: PackageDef = {

    BLOCK(
      OBJECTDEF(moduleSymbols.obj) := BLOCK(
        file.messageTypeList.flatMap { m =>
          val sym = symbolTable.messages(m)
          Vector[Tree](OBJECTDEF(sym.obj), CASECLASSDEF(sym.cls) withParams ())
        }
      )
    ) inPackage (moduleSymbols.pkg)
  }

  def responseFile: Process[CodeGeneratorResponse.File] = Process {
    File.newBuilder
      .setName(targetFile)
      .setContent(treeToString(fileDef))
      .build
  }

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

