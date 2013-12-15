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

  val varIntEncoder: Encoder[Long] = new Encoder[Long] {

    def run(in: Long) = {
      val hiBits = in >>> 7
      val b = (in & 0x7F).toByte
      if (hiBits != 0) (b - 0x80).toByte #:: run(hiBits) else b #:: Stream.empty
    }

  }

  val sint64Decoder: Decoder[Long] = varIntDecoder.map((n: Long) => (n >>> 1) ^ -(n & 1))
  val sint64Encoder: Encoder[Long] = varIntEncoder.contramap((n: Long) => (n << 1) ^ (n >> 63))

  val int32Decoder: Decoder[Int] = varIntDecoder.map(_.toInt)
  val int32Encoder: Encoder[Int] = varIntEncoder.contramap(n => n)

  val sint32Decoder: Decoder[Int] = int32Decoder.map((n: Int) => (n >>> 1) ^ -(n & 1))
  val sint32Encoder: Encoder[Int] = int32Encoder.contramap((n: Int) => (n << 1) ^ (n >> 31))

  val boolDecoder: Decoder[Boolean] = varIntDecoder.map(_ != 0)
  val boolEncoder: Encoder[Boolean] = new Encoder[Boolean] {
    override def run(in: Boolean) = (if (in) 1.toByte else 0.toByte) #:: Stream.empty
  }

  // Wire Type 1
  private def fixedEncoder[T](n: Int)(implicit v: T => Long): Encoder[T] = new Encoder[T] {
    override def run(in: T) = loop(n, v(in))

    private def loop(rem: Int, in: Long): Stream[Byte] =
      if (rem == 0) Stream.empty
      else (in & 0xFF).toByte #:: loop(rem - 1, in >>> 8)
  }

  val fixed64Decoder: Decoder[Long] = new BytesDecoder(8).map(bs =>
    (bs :\ 0L)((b: Byte, acc: Long) => (acc << 8) | (b & 0xFF)))
  val fixed64Encoder: Encoder[Long] = fixedEncoder(8)

  val doubleDecoder: Decoder[Double] = fixed64Decoder.map((l: Long) => java.lang.Double.longBitsToDouble(l))
  val doubleEncoder: Encoder[Double] = fixed64Encoder.contramap((d: Double) => java.lang.Double.doubleToLongBits(d))

  // Wire Type 5
  val fixed32Decoder: Decoder[Int] = new BytesDecoder(4) map (bs =>
    (bs :\ 0)((b: Byte, acc: Int) => (acc << 8) | (b & 0xFF)))
  val fixed32Encoder: Encoder[Int] = fixedEncoder(4)

  val floatDecoder: Decoder[Float] = fixed32Decoder.map((i: Int) => java.lang.Float.intBitsToFloat(i))
  val floatEncoder: Encoder[Float] = fixed32Encoder.contramap((f: Float) => java.lang.Float.floatToIntBits(f))

  // Wire Type 2
  private val byteSeqDecoder: Decoder[IndexedSeq[Byte]] = for {
    length <- int32Decoder
    bytes <- BytesDecoder(length)
  } yield bytes

  private val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = new Encoder[IndexedSeq[Byte]] {
    override def run(bs: IndexedSeq[Byte]): Stream[Byte] =
      int32Encoder.run(bs.size) #::: bs.toStream
  }

  val stringDecoder: Decoder[String] = for {
    bytes <- byteSeqDecoder
    string <- Decoder.constant(Try(new String(bytes.toArray, "UTF-8")) match {
      case Success(str) => Good(str)
      case _            => Bad(Decoder.InvalidUtf8)
    })
  } yield string

  val stringEncoder: Encoder[String] = byteSeqEncoder.contramap(str => str.getBytes("UTF-8"))

  val bytesDecoder: Decoder[ByteString] = for {
    bytes <- byteSeqDecoder
  } yield ByteString(bytes.toArray)

  val bytesEncoder: Encoder[ByteString] = byteSeqEncoder

}
