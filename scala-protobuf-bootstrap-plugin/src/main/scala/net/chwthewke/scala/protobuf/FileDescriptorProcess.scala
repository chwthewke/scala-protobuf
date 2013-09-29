package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File

trait FileDescriptorProcess {

  import PluginOps._
  import treehugger.forest._
  import treehugger.forest.definitions._
  import treehuggerDSL._

  def file: FileDescriptorProto

  def targetFile: String = (pkg.split('.') :+ s"$className.scala").mkString("/")

  def pkg = file.javaPackage

  def className = file.javaOuterClassName

  def fileDef: PackageDef = {
    val classSymbol = RootClass.newModuleClass(className)

    BLOCK(
      OBJECTDEF(classSymbol)
    ) inPackage (pkg)
  }

  def responseFile: Process[CodeGeneratorResponse.File] = Process {
    File.newBuilder
      .setName(targetFile)
      .setContent(treeToString(fileDef))
      .build
  }

}

object FileDescriptorProcess {
  def apply(file_ : FileDescriptorProto): Process[CodeGeneratorResponse.File] =
    new FileDescriptorProcess { override val file = file_ }.responseFile
}

