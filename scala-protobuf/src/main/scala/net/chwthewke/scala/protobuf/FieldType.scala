package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf
import scalautils._

sealed trait FieldType[T] {
  def wireType: Int

  def decoder: Decoder[T]
}

object FieldType {

  import Decoder._
  import DecoderOps._

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
    override lazy val decoder = for {
      ord <- WireFormat.int32Decoder
      res <- constant(enumByNumber(ord))
    } yield res

    def enumByNumber(ord: Int) = enum.numbered(ord) match {
      case Some(e) => Good(e)
      case None => Bad(InvalidEnumValue)
    }
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

  class Message[M](M: => protobuf.Message[M]) extends FieldType[M] {
    override val wireType = 2
    override lazy val decoder = for {
      updates <- anyFieldDecoder.untilEmpty.lengthPrefixed
    } yield Builder[M](updates: _*).build(M)

    private def fieldPart[C, T, M](f: => Field[C, T, M]): Decoder[FieldUpdate[M]] = for {
      value <- f.fieldType.decoder
    } yield f <+= value

    private def packedField[C, M](f: => Repeated[C, M]): Decoder[FieldUpdate[M]] = for {
      values <- f.fieldType.decoder.packed
    } yield f <++= values

    private def field(key: Int): Field[_, _, M] Or DecoderError =
      (M.fields.find(_.number == key >> 3), key & 0x7) match {
        case (Some(f), w) if f.fieldType.wireType == w => Good(f)
        case (Some(_), _) => Bad(WireTypeMismatch)
        case (None, _) => Bad(MalformedProtobuf) // TODO unknown fields
      }

    private def fieldDecoder(field: Field[_, _, M]): Decoder[FieldUpdate[M]] = field match {
      case rep: Repeated[_, M] if rep.packed => packedField(rep)
      case f => fieldPart(f)
    }

    def anyFieldDecoder: Decoder[FieldUpdate[M]] = for {
      key <- WireFormat.int32Decoder
      field <- constant(field(key))
      update <- fieldDecoder(field)
    } yield update
  }

}
