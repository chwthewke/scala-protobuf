package net.chwthewke.scala.protobuf.symbols


import com.google.protobuf.DescriptorProtos.{DescriptorProto, FileDescriptorProto}
import net.chwthewke.scala.protobuf
import treehugger.forest._
import treehugger.forest.definitions._
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait SymbolTableProcess {

  import protobuf._
  import PluginOps._

  def apply(): Process[SymbolTable] = for {
    req <- Process.ask :+> "Computing symbol table"
    fileSymbolTables <- req.protoFileList.map(processFile).sequence
  } yield fileSymbolTables.reduce(_ ++ _)

  def processFile(file: FileDescriptorProto) : Process[SymbolTable] = {
    val pkg = ( RootClass /: file.javaPackage.split('.') ) (_.newModuleClass(_))
    for {
      outerClassSymbol <- Process(pkg.newModuleClass(file.javaOuterClassName)) :+>> (s => s"ST: ${file.name} -> $s")
      messagesByFile <- file.messageTypeList.map(m => processMessage(outerClassSymbol, m)).sequence
      messages = messagesByFile.flatten
    } yield SymbolTable(Map(file -> outerClassSymbol), messages.toMap)
  }



  def processMessage(parent: Symbol, message: DescriptorProto): Process[Seq[(DescriptorProto, MessageSymbols)]] = {
    for {
      messageSymbols <- Process{
        MessageSymbols(parent.newClass(message.name), parent.newModuleClass(message.name))
      } :+>> (s => s"ST:   ${message.name} -> $s")
      nestedByParent <- message.nestedTypeList.map(processMessage(messageSymbols.obj, _)).sequence
      nested = nestedByParent.flatten
    } yield Seq(message -> messageSymbols) ++ nested
  }

}

object SymbolTableProcess extends SymbolTableProcess
