package net.chwthewke.scala.protobuf.bsplugin

import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTableProcess
import net.chwthewke.scala.protobuf.bsplugin.interface._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import net.chwthewke.scala.protobuf.bsplugin.templates.ProtoDef
import net.chwthewke.scala.protobuf.bsplugin.templates.TemplatesProcess
import scalaz.std.vector._

trait Plugin {

  def process: Process[CodeGeneratorResponse] = {

    for {
      req <- Process.ask :+> "Scala-Protobuf plugin started"
      protoSymbolTable <- ProtoSymbolTableProcess()
      filesToGenerate <- filesToGenerate
      protoDefs <- TemplatesProcess(protoSymbolTable, filesToGenerate).apply
    } yield CodeGeneratorResponse(protoDefs map toFile, None)
  }

  def toFile(protoDef: ProtoDef): File = File(protoDef.file, protoDef.all)

  def filesToGenerate: Process[Vector[FileDescriptor]] = {
    val result: Process[Vector[FileDescriptor]] = for {
      req <- Process.ask
    } yield req.protoFiles.filter((fdp: FileDescriptor) => req.filesToGenerate.contains(fdp.name))

    result :+>> (
      (l: Vector[FileDescriptor]) => s"Generating source for ${l.map(_.name).mkString(", ")}")
  }

}

object Plugin extends Plugin
