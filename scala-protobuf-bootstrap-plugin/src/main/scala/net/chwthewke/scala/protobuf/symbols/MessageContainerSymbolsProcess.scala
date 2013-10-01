package net.chwthewke.scala.protobuf.symbols

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, EnumValueDescriptorProto, FileDescriptorProto }
import treehugger.forest._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import net.chwthewke.scala.protobuf.MessageContainer

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
