package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, EnumValueDescriptorProto, FileDescriptorProto }
import treehugger.forest._
import treehugger.forest.definitions._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait SymbolTableProcess {

  import net.chwthewke.scala.protobuf._
  import PluginOps._

  def apply(): Process[SymbolTable] = for {
    req <- Process.ask :+> "Computing symbol table"
    fileSymbolTables <- req.protoFileList.map(processFile).sequence
  } yield fileSymbolTables.reduce(_ ++ _)

  def processFile(file: FileDescriptorProto): Process[SymbolTable] = {
    for {
      fileSymbols <- fileSymbols(file) :+>> (s => s"ST: ${file.name} -> $s")
      nested <- new MessageContainerSymbolsProcess(file, fileSymbols.obj, 1).processMessages
      (messages, enums) = nested
      descriptorPaths <- DescriptorPathsProcess()
      (messagePaths, enumPaths) = descriptorPaths
    } yield SymbolTable(Map(file -> fileSymbols),
      SymbolPaths(messages.toMap, messagePaths.map(_.swap)),
      SymbolPaths(enums.toMap, enumPaths.map(_.swap)))
  }

  private def fileSymbols(file: FileDescriptorProto) = Process {
    val pkg = RootClass.newModuleClass(file.javaPackage)
    FileSymbols(pkg.newModuleClass(file.javaOuterClassName), pkg)
  }

}

object SymbolTableProcess extends SymbolTableProcess {
  type CombinedMessageSymbols = (Vector[DescriptorSymbols], Vector[EnumDescriptorSymbols])

  type DescriptorSymbols = (DescriptorProto, MessageSymbols)
  type EnumDescriptorSymbols = (EnumDescriptorProto, EnumSymbols)
  type EnumValueDescriptorSymbols = (EnumValueDescriptorProto, ModuleClassSymbol)
}
