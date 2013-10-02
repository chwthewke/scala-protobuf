package net.chwthewke.scala.protobuf

import treehugger.forest._
import treehugger.forest.definitions._

package object symbols {
  val ByteStringClass: Symbol =
    definitions.getClass(classOf[ByteString].getCanonicalName)
}
