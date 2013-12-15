package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils._

trait ProtobufDecoder[M] {

  import FieldType._
  import WireFormat._

  implicit val M: Message[M]

  def builderDecoder: Decoder[M.Builder] = anyFieldDecoder.untilEmpty map { ups: Vector[M.Field#Update] =>
    M.newBuilder.mod(ups: _*)
  }

  def decoder: Decoder[M] = builderDecoder map (_.build) named (_ => s"$M")

  private[protobuf] def anyFieldDecoder: Decoder[M.Field#Update] = (for {
    field <- fieldHeaderDecoder flatMap { case (number, wireType) => Decoder.constant(field(number, wireType)) }
    fieldDecoder <- fieldDecoder(field)
  } yield fieldDecoder) named (_ => s"Any field of $M")

  private[protobuf] def fieldDecoder(field: M.Field): Decoder[field.Update] = field match {
    case packed: M.Packed[_] => packedFieldDecoder(field) named (_ => s"$M.${field.name} (packed)")
    case _                   => nonPackedFieldDecoder(field) named (_ => s"$M.${field.name}")
  }

  private def nonPackedFieldDecoder(field: M.Field): Decoder[field.Update] = for {
    value <- valueDecoder(field.fieldType) named (s"Value of $M.${field.name} " + _)
  } yield field add value

  private def packedFieldDecoder[T](field: M.Field): Decoder[field.Update] = for {
    length <- int32Decoder
    values <- valueDecoder(field.fieldType).untilEmpty.from(Decoder.byteSource(length))(identity)
  } yield field addAll values

  private def valueDecoder[T](fieldType: FieldType[T]): Decoder[T] = {
    val dec: Decoder[T] = fieldType match {
      case Int64 | UInt64     => varIntDecoder
      case SInt64             => sint64Decoder
      case Int32 | UInt32     => int32Decoder
      case SInt32             => sint32Decoder
      case Bool               => boolDecoder
      case Fixed64 | SFixed64 => fixed64Decoder
      case Double             => doubleDecoder
      case Fixed32 | SFixed32 => fixed32Decoder
      case Float              => floatDecoder
      case Bytes              => bytesDecoder
      case String             => stringDecoder
      case Enum(pe)           => int32Decoder flatMap (x => Decoder.constant(enumValue(pe)(x)))
      case MessageField(m)    => ProtobufDecoder(m).decoder
    }

    dec named (_ => s"$fieldType")
  }

  private def enumValue[T](pe: ProtobufEnum[T])(v: Int): T Or Decoder.Error =
    pe.numbered(v) match {
      case Some(e) => Good(e)
      case None    => Bad(Decoder.InvalidEnumValue)
    }

  private def fieldHeaderDecoder: Decoder[(Int, WireType.t)] =
    int32Decoder map (x => (x >> 3, x & 0x07))

  private def field(number: Int, wireType: Int): M.Field Or Decoder.Error =
    M.fields find (_.number == number) match {
      case None                              => Bad(Decoder.MalformedProtobuf)
      case Some(f) if f.wireType == wireType => Good(f)
      case _                                 => Bad(Decoder.WireTypeMismatch)
    }

}

object ProtobufDecoder {
  def apply[M](implicit M0: Message[M]): ProtobufDecoder[M] = new ProtobufDecoder[M] { override implicit val M = M0 }
}
