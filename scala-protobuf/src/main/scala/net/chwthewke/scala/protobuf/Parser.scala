package net.chwthewke.scala.protobuf

import scalautils.Bad
import scalautils.Good
import scalautils.Or

trait Parser[M] extends Decoder[M] {
  import Decoder._

  implicit def M: Message[M]

  override def run(source: ByteSource): (ByteSource, M Or DecoderError) = readMessage.run(source)

  def lengthPrefixed: Decoder[M] = readMessageBytes.into(readMessage)

  private def readMessageBytes: Decoder[ByteSource] = for {
    length <- WireFormat.int32Decoder
    bytes <- WireFormat.BytesDecoder(length)
  } yield bytes

  private def readMessage: Decoder[M] = for {
    updates <- readAnyField.untilEmpty
  } yield Builder[M](updates: _*).build

  private def readAnyField: Decoder[FieldUpdate[M]] = {
    for {
      (field, wireType) <- WireFormat.varIntDecoder.map(_.toInt).blindMap(fieldKey)
      if field.fieldType.wireType == wireType
      update <- readField(field)
    } yield update
  }

  private def readField[C, T](field: Field[C, T, M]): Decoder[Field[C, T, M]#Update] =
    for (value <- field.fieldType.decoder) yield field <+= value

  private def fieldKey(key: Int): (Field[_, _, M], Int) Or DecoderError = (field(key >> 3), key & 0x7) match {
    case (Some(f), w) => Good((f, w))
    case (None, _) => Bad(MalformedProtobuf) // TODO unknown fields
  }

  private def field(number: Int): Option[Field[_, _, M]] = M.fields.find(_.number == number)

}

object Parser {
  def apply[M](implicit M0: Message[M]) = new Parser[M] { override implicit def M = M0 }
}

