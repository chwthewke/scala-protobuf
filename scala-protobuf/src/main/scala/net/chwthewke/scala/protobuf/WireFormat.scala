package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils.{Bad, Good, Or}
import scala.annotation.tailrec
import scala.util.{Success, Try}

object WireFormat {
  import Decoder._

  private[protobuf] case class BytesDecoder(n: Int) extends Decoder[IndexedSeq[Byte]] {
    def run(source: ByteSource): (ByteSource, IndexedSeq[Byte] Or DecoderError) =
      source.getBytes(n) match {
        case (src, Good(bs)) => (src, Good(bs))
        case (src, _) => (src, Bad(TruncatedProtobuf))
      }
  }

  // Wire Type 0
  val varIntDecoder: Decoder[Long] = new Decoder[Long] {
    @tailrec
    private def loop(
                      source: ByteSource,
                      read: Int,
                      acc: Long): (ByteSource, Int, Long Or DecoderError) = {

      if (read > 9) (source, read, Bad(TooLongVarInt))
      else source.getByte match {
        case (src, Good(x)) => {
          val next = (x & 0x7F) << (read * 7) | acc
          if (x < 0) loop(src, read + 1, next)
          else (src, read + 1, Good(next))
        }
        case (src, _) => (src, read, Bad(TruncatedVarInt))
      }
    }

    def run(src: ByteSource) = loop(src, 0, 0L) match {
      case (st, _, a) => (st, a)
    }
  }

  val int64Decoder: Decoder[Long] = varIntDecoder
  val uint64Decoder: Decoder[Long] = varIntDecoder
  val sint64Decoder: Decoder[Long] = uint64Decoder.map((n: Long) => (n >>> 1) ^ -(n & 1))

  val int32Decoder: Decoder[Int] = varIntDecoder.map(_.toInt)
  val uint32Decoder: Decoder[Int] = int32Decoder
  val sint32Decoder: Decoder[Int] = uint32Decoder.map((n: Int) => (n >>> 1) ^ -(n & 1))

  val boolDecoder: Decoder[Boolean] = varIntDecoder.map(_ != 0)

  // Wire Type 1
  val fixed64Decoder: Decoder[Long] = new BytesDecoder(8).map(bs => {
    val longs = bs.map(_.toLong)
    longs(0) |
      (longs(1) << 8) |
      (longs(2) << 16) |
      (longs(3) << 24) |
      (longs(4) << 32) |
      (longs(5) << 40) |
      (longs(6) << 48) |
      (longs(7) << 56)
  })
  val sfixed64Decoder: Decoder[Long] = fixed64Decoder
  val doubleDecoder: Decoder[Double] = fixed64Decoder.map((l: Long) => java.lang.Double.longBitsToDouble(l))

  // Wire Type 5
  val fixed32Decoder: Decoder[Int] = new BytesDecoder(4).map(bs =>
    bs(0) |
      (bs(1) << 8) |
      (bs(2) << 16) |
      (bs(3) << 24))
  val sfixed32Decoder: Decoder[Int] = fixed32Decoder

  val floatDecoder: Decoder[Float] = fixed32Decoder.map((i: Int) => java.lang.Float.intBitsToFloat(i))

  private val byteSeqDecoder: Decoder[IndexedSeq[Byte]] = for {
    length <- int32Decoder
    bytes <- BytesDecoder(length)
  } yield bytes

  val stringDecoder: Decoder[String] = for {
    bytes <- byteSeqDecoder
    string <- Decoder.blind(Try(new String(bytes.toArray, "UTF-8")) match {
      case Success(str) => Good(str)
      case _ => Bad(InvalidUtf8)
    })
  } yield string

  val bytesDecoder: Decoder[ByteString] = for {
    bytes <- byteSeqDecoder
  } yield ByteString(bytes.toArray)

}
