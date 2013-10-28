package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf
import scalautils._

sealed trait FieldType[T] {
  def wireType: Int
  def decoder: Decoder[T]
}

object FieldType {
  import Decoder._

  case object Int64 extends FieldType[Long] {
    override val wireType = 0
    override val decoder = WireFormat.int64Decoder
  }

  case object UInt64 extends FieldType[Long] {
    override val wireType = 0
    override val decoder = WireFormat.uint64Decoder
  }

  case object SInt64 extends FieldType[Long] {
    override val wireType = 0
    override val decoder = WireFormat.sint64Decoder
  }

  case object Int32 extends FieldType[Int] {
    override val wireType = 0
    override val decoder = WireFormat.int32Decoder
  }

  case object UInt32 extends FieldType[Int] {
    override val wireType = 0
    override val decoder = WireFormat.uint32Decoder
  }

  case object SInt32 extends FieldType[Int] {
    override val wireType = 0
    override val decoder = WireFormat.sint32Decoder
  }

  case object Bool extends FieldType[Boolean] {
    override val wireType = 0
    override val decoder = WireFormat.boolDecoder
  }

  class Enum[M <: Numbered](enum: => ProtobufEnum[M]) extends FieldType[M] {
    override val wireType = 0
    override lazy val decoder = WireFormat.int32Decoder
      .blindMap(enum.numbered(_).fold[M Or DecoderError](Bad(InvalidEnumValue))(Good(_)))
  }

  case object Fixed64 extends FieldType[Long] {
    override val wireType = 1
    override val decoder = WireFormat.fixed64Decoder
  }

  case object SFixed64 extends FieldType[Long] {
    override val wireType = 1
    override val decoder = WireFormat.sfixed64Decoder
  }

  case object Double extends FieldType[Double] {
    override val wireType = 1
    override val decoder = WireFormat.doubleDecoder
  }

  case object Fixed32 extends FieldType[Int] {
    override val wireType = 5
    override val decoder = WireFormat.fixed32Decoder
  }

  case object SFixed32 extends FieldType[Int] {
    override val wireType = 5
    override val decoder = WireFormat.sfixed32Decoder
  }

  case object Float extends FieldType[Float] {
    override val wireType = 5
    override val decoder = WireFormat.floatDecoder
  }

  case object Bytes extends FieldType[ByteString] {
    override val wireType = 2
    override val decoder = WireFormat.bytesDecoder
  }

  case object String extends FieldType[String] {
    override val wireType = 2
    override val decoder = WireFormat.stringDecoder
  }

  class Message[M](m: => protobuf.Message[M]) extends FieldType[M] {
    override val wireType = 2
    override lazy val decoder = Parser[M](m).lengthPrefixed
  }

}
