package net.chwthewke.scala.protobuf.bsplugin

import treehugger.forest._
import treehugger.forest.definitions._

package object symbols {

  val RuntimePackage: Symbol = getModule("net.chwthewke.scala.protobuf").moduleClass

  val ByteStringClass: Symbol = RuntimePackage.newClass("ByteString")

  val MessageTrait: ClassSymbol = RuntimePackage.newClass("Message")
  val MessageSyntaxTrait: ClassSymbol = RuntimePackage.newClass("MessageSyntax")

  val BuilderClass: ClassSymbol = RuntimePackage.newClass("Builder")

  val FieldTrait: ClassSymbol = RuntimePackage.newClass("Field")
  val RequiredObject: ModuleClassSymbol = RuntimePackage.newModuleClass("Required")
  val OptionalObject: ModuleClassSymbol = RuntimePackage.newModuleClass("Optional")
  val RepeatedObject: ModuleClassSymbol = RuntimePackage.newModuleClass("Repeated")

}
