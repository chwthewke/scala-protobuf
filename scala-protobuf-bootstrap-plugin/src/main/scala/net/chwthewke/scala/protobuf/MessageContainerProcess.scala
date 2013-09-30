package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.symbols.SymbolTable
import treehugger._
import treehugger.forest._
import com.google.protobuf.DescriptorProtos.{ FileDescriptorProto, DescriptorProto }
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait MessageContainerProcess[A] {

  import MessageContainer._

  implicit def MC: MessageContainer[A]

  def self: A

  def symbolTable: SymbolTable

  def apply: Process[Vector[Tree]] = {

    for {
      byMessage <- self.messages.map(DescriptorProcess(_, symbolTable)).sequence
    } yield byMessage.flatten
  }

}

object MessageContainerProcess {
  def apply[A: MessageContainer](mc: A, sym: SymbolTable) = new MessageContainerProcess[A] {
    def MC = MessageContainer[A]
    def self = mc
    def symbolTable = sym
  }.apply
}
