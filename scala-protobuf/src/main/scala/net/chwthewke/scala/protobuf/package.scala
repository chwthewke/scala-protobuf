package net.chwthewke.scala

import scala.language.existentials

package object protobuf {
  type ByteString = akka.util.ByteString
  val ByteString: akka.util.ByteString.type = akka.util.ByteString
}
