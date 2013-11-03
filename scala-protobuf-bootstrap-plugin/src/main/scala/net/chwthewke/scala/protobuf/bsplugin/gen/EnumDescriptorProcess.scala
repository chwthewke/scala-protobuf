package net.chwthewke.scala.protobuf.bsplugin.gen

import com.google.protobuf.DescriptorProtos._
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import net.chwthewke.scala.protobuf.bsplugin.symbols._
import treehugger.forest._
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._

trait EnumDescriptorProcess {

  def self: EnumDescriptorProto

  def symbolTable: ProtoSymbolTable

  lazy val symbol: EnumSymbol = symbolTable.enum(self).get

  def apply: Process[Vector[Tree]] = process {

    val enumClassSymbol = symbol.cls
    val classDef: Tree = (CLASSDEF(enumClassSymbol)
      withParams (VAL("name", StringClass), VAL("number", IntClass))
      withFlags (Flags.SEALED, Flags.ABSTRACT)
      withParents (enumClassSymbol DOT "Value", REF(NumberedTrait)))

    def valueObjectDef(descriptor: EnumValueDescriptorProto, obj: ModuleClassSymbol): Tree = {
      (CASEOBJECTDEF(obj)
        withParents (enumClassSymbol APPLY (LIT(descriptor.name), LIT(descriptor.number))))
    }

    def valuesValDef: Tree = (VAL("values", appliedType(VectorClass, enumClassSymbol)) := (
      VectorClass.module DOT nme.apply) APPLYTYPE enumClassSymbol APPLY (symbol.values.values.map(REF)))

    def protobufEnumType = appliedType(ProtobufEnumTrait, enumClassSymbol)

    def implicitProtobufEnumDef: Tree = (VAL("protobufEnumInstance", protobufEnumType)
      withFlags (Flags.IMPLICIT) := THIS)

    val objectDef: Tree =
      (OBJECTDEF(enumClassSymbol)
        withParents (protobufEnumType) := BLOCK(
          symbol.values.toVector.map((valueObjectDef _).tupled) :+
            valuesValDef :+
            implicitProtobufEnumDef
        ))

    Vector[Tree](classDef, objectDef)
  }

}

object EnumDescriptorProcess {
  def apply(enum: EnumDescriptorProto, sym: ProtoSymbolTable): Process[Vector[Tree]] =
    new EnumDescriptorProcess { def self = enum; def symbolTable = sym }.apply
}
