package net.chwthewke.scala

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import scalaz.ReaderWriterState

package object protobuf {

  type Process[X] = ReaderWriterState[CodeGeneratorRequest, Vector[String], Unit, X]

  type ProcessW[W, X] = ReaderWriterState[CodeGeneratorRequest, W, Unit, X]

  object syntax extends ProcessSyntax with MessageContainerSyntax with PluginOps

  val Process = syntax.Process
}
