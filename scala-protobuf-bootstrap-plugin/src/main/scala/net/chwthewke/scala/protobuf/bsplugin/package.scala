package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import scalaz.ReaderWriterState

package object bsplugin {

  type Process[X] = ReaderWriterState[CodeGeneratorRequest, Vector[String], Unit, X]

  type ProcessW[W, X] = ReaderWriterState[CodeGeneratorRequest, W, Unit, X]

  object syntax extends ProcessSyntax with MessageContainerSyntax with PluginSyntax

  val process: syntax.Process.type = syntax.Process
}
