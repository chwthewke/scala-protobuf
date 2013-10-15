package net.chwthewke.scala.protobuf.bsplugin

import treehugger.forest._
import treehugger.forest.definitions._

package object symbols {

  val RuntimePackage: Symbol = getModule("net.chwthewke.scala.protobuf").moduleClass

  val ByteStringClass: Symbol = RuntimePackage.newClass("ByteString")

}
