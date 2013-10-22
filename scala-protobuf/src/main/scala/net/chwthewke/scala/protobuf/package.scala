package net.chwthewke.scala

import scala.language.existentials

package object protobuf {
  type FieldUpdate[M] = Field[x, y, M]#Update forSome { type x; type y }

  type ByteString = akka.util.ByteString
}
