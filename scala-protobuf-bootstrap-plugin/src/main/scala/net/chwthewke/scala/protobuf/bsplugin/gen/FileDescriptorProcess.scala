package net.chwthewke.scala.protobuf.bsplugin.gen

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.symbols.FileSymbol
import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTable
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait FileDescriptorProcess {

  import treehugger.forest._
  import treehugger.forest.definitions._
  import treehuggerDSL._

  def self: FileDescriptorProto

  def symbolTable: ProtoSymbolTable

  def moduleName = symbol.obj.nameString

  def targetFile: String = (self.javaPackage.split('.') :+ s"$moduleName.scala").mkString("/")

  lazy val symbol: FileSymbol = symbolTable.file(self).get

  def fileDef: Process[PackageDef] = {
    for {
      messageDefs <- MessageContainerProcess(self, symbolTable)
    } yield BLOCK(
      OBJECTDEF(symbol.obj) := BLOCK(
        messageDefs
      )
    ) inPackage (symbol.pkg)
  }

  def responseFile: Process[CodeGeneratorResponse.File] = for {
    fileDef <- this.fileDef
  } yield File.newBuilder
    .setName(targetFile)
    .setContent(treeToString(fileDef))
    .build

}

object FileDescriptorProcess {
  def apply(sym: ProtoSymbolTable, f: FileDescriptorProto): Process[CodeGeneratorResponse.File] =
    new FileDescriptorProcess {
      override val self = f
      override val symbolTable = sym
    }.responseFile
}

