package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils._
import scala.annotation.tailrec
import scala.util.{ Success, Try }

object WireFormat {

  private[protobuf] case class BytesDecoder(n: Int) extends Decoder[IndexedSeq[Byte]] {
    def run(source: ByteSource): (ByteSource, IndexedSeq[Byte] Or Decoder.Error) =
      source.getBytes(n) match {
        case (src, Good(bs)) => (src, Good(bs))
        case (src, _)        => (src, Bad(Decoder.TruncatedProtobuf))
      }
  }

  // Wire Type 0
  val varIntDecoder: Decoder[Long] = new Decoder[Long] {
    @tailrec
    private def loop(
      source: ByteSource,
      read: Int,
      acc: Long): (ByteSource, Int, Long Or Decoder.Error) = {

      if (read > 9) (source, read, Bad(Decoder.TooLongVarInt))
      else source.getByte match {
        case (src, Good(x)) => {
          val next = (x & 0x7FL) << (read * 7) | acc
          if (x < 0) loop(src, read + 1, next)
          else (src, read + 1, Good(next))
        }
        case (src, _) => (src, read, Bad(Decoder.TruncatedVarInt))
      }
    }

    def run(src: ByteSource) = loop(src, 0, 0L) match {
      case (st, _, a) => (st, a)
    }
  }

  val sint64Decoder: Decoder[Long] = varIntDecoder.map((n: Long) => (n >>> 1) ^ -(n & 1))

  val int32Decoder: Decoder[Int] = varIntDecoder.map(_.toInt)

  val sint32Decoder: Decoder[Int] = int32Decoder.map((n: Int) => (n >>> 1) ^ -(n & 1))

  val boolDecoder: Decoder[Boolean] = varIntDecoder.map(_ != 0)

  // Wire Type 1
  val fixed64Decoder: Decoder[Long] = new BytesDecoder(8).map(bs =>
    (bs :\ 0L)((b: Byte, acc: Long) => (acc << 8) | (b & 0xFF)))

  val doubleDecoder: Decoder[Double] = fixed64Decoder.map((l: Long) => java.lang.Double.longBitsToDouble(l))

  // Wire Type 5
  val fixed32Decoder: Decoder[Int] = new BytesDecoder(4) map (bs =>
    (bs :\ 0)((b: Byte, acc: Int) => (acc << 8) | (b & 0xFF)))

  val floatDecoder: Decoder[Float] = fixed32Decoder.map((i: Int) => java.lang.Float.intBitsToFloat(i))

  // Wire Type 2
  private val byteSeqDecoder: Decoder[IndexedSeq[Byte]] = for {
    length <- int32Decoder
    bytes <- BytesDecoder(length)
  } yield bytes

  val stringDecoder: Decoder[String] = for {
    bytes <- byteSeqDecoder
    string <- Decoder.constant(Try(new String(bytes.toArray, "UTF-8")) match {
      case Success(str) => Good(str)
      case _            => Bad(Decoder.InvalidUtf8)
    })
  } yield string

  val bytesDecoder: Decoder[ByteString] = for {
    bytes <- byteSeqDecoder
  } yield ByteString(bytes.toArray)

}
