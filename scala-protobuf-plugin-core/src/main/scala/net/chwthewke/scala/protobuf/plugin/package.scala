package net.chwthewke.scala.protobuf

import scalaz.ReaderWriterState

package object plugin {
  import interface._

  type IndexedSeq[+X] = scala.collection.immutable.IndexedSeq[X]

  type Process[X] = ReaderWriterState[CodeGeneratorRequest, Vector[String], Unit, X]

  type ProcessW[W, X] = ReaderWriterState[CodeGeneratorRequest, W, Unit, X]

  object syntax extends ProcessSyntax with MessageContainerSyntax

  val process: syntax.Process.type = syntax.Process
}
