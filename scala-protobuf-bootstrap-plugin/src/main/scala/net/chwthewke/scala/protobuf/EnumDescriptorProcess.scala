package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.EnumDescriptorProto
import net.chwthewke.scala.protobuf.symbols.EnumSymbols
import net.chwthewke.scala.protobuf.symbols.SymbolTable
import treehugger.forest._
import treehugger.forest.treehuggerDSL._
import net.chwthewke.scala.protobuf.symbols.SymbolTable

trait EnumDescriptorProcess {

  import PluginOps._

  def self: EnumDescriptorProto

  def symbolTable: SymbolTable

  def apply: Process[Vector[Tree]] = Process {
    val enumSymbols: EnumSymbols = symbolTable.enums.symbols(self)
    val enumClassSymbol = enumSymbols.enum
    val classDef: Tree = CLASSDEF(enumClassSymbol).withFlags(Flags.SEALED, Flags.ABSTRACT)
    classDef +: self.valueList.map { v =>
      CASEOBJECTDEF(enumSymbols.values(v)).withParents(enumClassSymbol): Tree
    }
  }

}

object EnumDescriptorProcess {
  def apply(enum: EnumDescriptorProto, sym: SymbolTable): Process[Vector[Tree]] =
    new EnumDescriptorProcess { def self = enum; def symbolTable = sym }.apply
}
