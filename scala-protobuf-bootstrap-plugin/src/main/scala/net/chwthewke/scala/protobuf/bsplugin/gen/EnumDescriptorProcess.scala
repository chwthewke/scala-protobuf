package net.chwthewke.scala.protobuf.bsplugin.gen

import com.google.protobuf.DescriptorProtos.EnumDescriptorProto
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.symbols.EnumSymbol
import net.chwthewke.scala.protobuf.bsplugin.symbols.ProtoSymbolTable
import treehugger.forest._
import treehugger.forest.treehuggerDSL._

trait EnumDescriptorProcess {

  def self: EnumDescriptorProto

  def symbolTable: ProtoSymbolTable

  lazy val symbol: EnumSymbol = symbolTable.enum(self).get

  def apply: Process[Vector[Tree]] = process {

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
