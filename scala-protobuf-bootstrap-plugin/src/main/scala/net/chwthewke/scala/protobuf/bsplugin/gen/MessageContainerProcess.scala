package net.chwthewke.scala.protobuf.bsplugin.gen

import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTable
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import treehugger._
import treehugger.forest._

trait MessageContainerProcess[A] {

  implicit def MC: MessageContainer[A]

  def self: A

  def symbolTable: ProtoSymbolTable

  def apply: Process[Vector[Tree]] = {

    for {
      byMessage <- self.messages.map(DescriptorProcess(_, symbolTable)).sequence
    } yield byMessage.flatten
  }

}

object MessageContainerProcess {
  def apply[A: MessageContainer](mc: A, sym: ProtoSymbolTable) = new MessageContainerProcess[A] {
    def MC = MessageContainer[A]
    def self = mc
    def symbolTable = sym
  }.apply
}
