package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, EnumValueDescriptorProto, FileDescriptorProto }
import treehugger.forest._
import treehugger.forest.definitions._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import net.chwthewke.scala.protobuf.MessageContainer

trait SymbolTableProcess {

  import net.chwthewke.scala.protobuf._
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
      nested <- new MessageContainerSymbolsProcess(file, fileSymbols.obj, 1).processMessages
      (messages, enums) = nested
      descriptorPaths <- DescriptorPathsProcess()
    } yield SymbolTable(Map(file -> fileSymbols), messages.toMap, enums.toMap, descriptorPaths)
  }

  private def fileSymbols(file: FileDescriptorProto) = Process {
    val pkg = RootClass.newModuleClass(file.javaPackage)
    FileSymbols(pkg.newModuleClass(file.javaOuterClassName), pkg)
  }

}

class MessageContainerSymbolsProcess[A: MessageContainer](
  val container: A, val containerSymbol: ModuleClassSymbol, val logIdent: Int) {

  import net.chwthewke.scala.protobuf._
  import SymbolTableProcess._
  import MessageContainer._
  import PluginOps._

  def processMessages: Process[CombinedMessageSymbols] = {
    for {
      enums <- processEnums
      nestedSymbols <- container.messages.map(processMessage).sequence
      (nestedMessages, nestedEnums) = nestedSymbols.unzip
    } yield (nestedMessages.flatten, enums ++ nestedEnums.flatten)
  }

  private def processMessage(descriptor: DescriptorProto): Process[CombinedMessageSymbols] = {
    for {
      messageSymbols <- messageSymbols(descriptor) :+>> (s => s"ST: ${" " * logIdent}${descriptor.name} -> $s")
      nested <- new MessageContainerSymbolsProcess(descriptor, messageSymbols.obj, logIdent + 1).processMessages
      (nestedMessages, nestedEnums) = nested
    } yield ((descriptor -> messageSymbols) +: nestedMessages, nestedEnums)
  }

  private def messageSymbols(message: DescriptorProto) = Process {
    MessageSymbols(containerSymbol.newClass(message.name), containerSymbol.newModuleClass(message.name))
  }

  private def processEnums: Process[Vector[EnumDescriptorSymbols]] = {
    container.enums.map(processEnum).sequence
  }

  private def processEnum(enum: EnumDescriptorProto): Process[EnumDescriptorSymbols] = {

    def processEnumValue(value: EnumValueDescriptorProto): Process[EnumValueDescriptorSymbols] =
      Process(value -> containerSymbol.newModuleClass(value.name))

    for {
      enumSymbol <- Process(containerSymbol.newClass(enum.name)) :+>> (s => s"ST: ${" " * logIdent}${enum.name} -> $s")
      valueSymbols <- enum.valueList.map(processEnumValue).sequence
    } yield enum -> EnumSymbols(enumSymbol, valueSymbols.toMap)
  }

}

object SymbolTableProcess extends SymbolTableProcess {
  type CombinedMessageSymbols = (Vector[DescriptorSymbols], Vector[EnumDescriptorSymbols])

  type DescriptorSymbols = (DescriptorProto, MessageSymbols)
  type EnumDescriptorSymbols = (EnumDescriptorProto, EnumSymbols)
  type EnumValueDescriptorSymbols = (EnumValueDescriptorProto, ModuleClassSymbol)
}
