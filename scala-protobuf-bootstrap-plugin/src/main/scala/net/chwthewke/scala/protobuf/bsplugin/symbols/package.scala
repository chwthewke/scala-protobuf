package net.chwthewke.scala.protobuf.bsplugin

import treehugger.forest._
import treehugger.forest.definitions._
import net.chwthewke.scala.protobuf.ByteString

package object symbols {
  val ByteStringClass: Symbol =
    definitions.getClass(classOf[ByteString].getCanonicalName)
}
