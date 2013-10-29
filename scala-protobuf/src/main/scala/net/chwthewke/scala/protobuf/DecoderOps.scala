package net.chwthewke.scala.protobuf

trait DecoderOps[A] {
  def self: Decoder[A]

  private def readLengthBytes: Decoder[ByteSource] = for {
    length <- WireFormat.int32Decoder
    bytes <- WireFormat.BytesDecoder(length)
  } yield bytes

  def lengthPrefixed: Decoder[A] = readLengthBytes.into(self)

  def packed: Decoder[Vector[A]] = readLengthBytes.into(self.untilEmpty)

}

object DecoderOps {
  implicit class ToDecoderOps[A](override val self: Decoder[A]) extends DecoderOps[A]
}