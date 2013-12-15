package net.chwthewke.scala.protobuf

sealed trait FieldType[T] {
  def wireType: WireType.t
}

object FieldType {

  case object Int64 extends FieldType[Long] {
    override val wireType = WireType.VarInt
  }

  case object UInt64 extends FieldType[Long] {
    override val wireType = WireType.VarInt
  }

  case object SInt64 extends FieldType[Long] {
    override val wireType = WireType.VarInt
  }

  case object Int32 extends FieldType[Int] {
    override val wireType = WireType.VarInt
  }

  case object UInt32 extends FieldType[Int] {
    override val wireType = WireType.VarInt
  }

  case object SInt32 extends FieldType[Int] {
    override val wireType = WireType.VarInt
  }

  case object Bool extends FieldType[Boolean] {
    override val wireType = WireType.VarInt
  }

  case class Enum[M](enum: ProtobufEnum[M]) extends FieldType[M] {
    override val wireType = WireType.VarInt
  }

  case object Fixed64 extends FieldType[Long] {
    override val wireType = WireType.Fixed64
  }

  case object SFixed64 extends FieldType[Long] {
    override val wireType = WireType.Fixed64
  }

  case object Double extends FieldType[Double] {
    override val wireType = WireType.Fixed64
  }

  case object Fixed32 extends FieldType[Int] {
    override val wireType = WireType.Fixed32
  }

  case object SFixed32 extends FieldType[Int] {
    override val wireType = WireType.Fixed32
  }

  case object Float extends FieldType[Float] {
    override val wireType = WireType.Fixed32
  }

  case object Bytes extends FieldType[ByteString] {
    override val wireType = WireType.LengthPrefixed
  }

  case object String extends FieldType[String] {
    override val wireType = WireType.LengthPrefixed
  }

  case class MessageField[M](M: Message[M]) extends FieldType[M] {
    override val wireType = WireType.LengthPrefixed
  }

}
