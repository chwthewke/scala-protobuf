package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf
import treehugger.forest._
import treehugger.forest.definitions._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait SymbolTableProcess {

  import protobuf._
  import PluginOps._
  import SymbolTableProcess._
  import MessageContainer._

  def apply(): Process[SymbolTable] = for {
    req <- Process.ask :+> "Computing symbol table"
    fileSymbolTables <- req.protoFileList.map(processFile).sequence
  } yield fileSymbolTables.reduce(_ ++ _)

  def processFile(file: FileDescriptorProto): Process[SymbolTable] = {
    for {
      fileSymbols <- fileSymbols(file) :+>> (s => s"ST: ${file.name} -> $s")
      messages <- processNestedMessages(file, fileSymbols.obj)
    } yield SymbolTable(Map(file -> fileSymbols), messages.toMap)
  }

  def processMessage(parent: ModuleClassSymbol, message: DescriptorProto): Process[Vector[DescriptorSymbols]] = {
    for {
      messageSymbols <- messageSymbols(parent, message) :+>> (s => s"ST:   ${message.name} -> $s")
      nested <- processNestedMessages(message, messageSymbols.obj)
    } yield (message -> messageSymbols) +: nested
  }

  private def messageSymbols(parent: ModuleClassSymbol, message: DescriptorProto) = Process {
    MessageSymbols(parent.newClass(message.name), parent.newModuleClass(message.name))
  }

  private def fileSymbols(file: FileDescriptorProto) = Process {
    val pkg = RootClass.newModuleClass(file.javaPackage)
    FileSymbols(pkg.newModuleClass(file.javaOuterClassName), pkg)
  }

  private def processNestedMessages[A](messageContainer: A,
    parentSymbol: ModuleClassSymbol)(implicit MC: MessageContainer[A]): Process[Vector[DescriptorSymbols]] = {
    for {
      byParent <- messageContainer.messages.map(processMessage(parentSymbol, _)).sequence
    } yield byParent.flatten
  }

}

object SymbolTableProcess extends SymbolTableProcess {
  type DescriptorSymbols = (DescriptorProto, MessageSymbols)
}
