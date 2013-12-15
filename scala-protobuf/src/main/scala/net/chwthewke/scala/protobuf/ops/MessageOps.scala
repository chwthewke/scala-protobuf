package net.chwthewke.scala.protobuf.ops

import net.chwthewke.scala.protobuf.Message
import net.chwthewke.scala.protobuf.ProtobufEncoder
import scala.language.implicitConversions
import net.chwthewke.scala.protobuf.ByteString

trait MessageOps[M] {

  implicit val M: Message[M]

  def self: M

  def toByteStream: Stream[Byte] =
    ProtobufEncoder[M].run(self)
}

trait ToMessageOps {
  implicit def toMessageOps[M](m: M)(implicit M0: Message[M]) =
    new MessageOps[M] {
      def self = m
      val M = M0
    }
}

object message extends ToMessageOps
