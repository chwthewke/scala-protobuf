package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.EnumDescriptorProto
import treehugger.forest._
import treehugger.forest.treehuggerDSL._
import net.chwthewke.scala.protobuf.symbols.ProtoSymbolTable
import net.chwthewke.scala.protobuf.symbols.EnumSymbol
import net.chwthewke.scala.protobuf.symbols.EnumSymbol

trait EnumDescriptorProcess {

  import PluginOps._

  def self: EnumDescriptorProto

  def symbolTable: ProtoSymbolTable

  lazy val symbol: EnumSymbol = symbolTable.symbols.collectFirst {
    case es @ EnumSymbol(_, _, _, enum, _, _) if enum == self => es
  }.get

  def apply: Process[Vector[Tree]] = Process {

    val enumClassSymbol = symbol.cls
    val classDef: Tree = CLASSDEF(enumClassSymbol).withFlags(Flags.SEALED, Flags.ABSTRACT)
    classDef +: symbol.values.toVector.map {
      case (_, obj) =>
        CASEOBJECTDEF(obj).withParents(enumClassSymbol): Tree
    }
  }

}

object EnumDescriptorProcess {
  def apply(enum: EnumDescriptorProto, sym: ProtoSymbolTable): Process[Vector[Tree]] =
    new EnumDescriptorProcess { def self = enum; def symbolTable = sym }.apply
}
