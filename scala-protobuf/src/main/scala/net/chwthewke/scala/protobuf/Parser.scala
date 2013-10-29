package net.chwthewke.scala.protobuf

import java.io.{ IOException, InputStream }
import net.chwthewke.scala.protobuf.scalautils.{ Good, Bad, Or }
import Decoder.DecoderError
import net.chwthewke.scala.protobuf.ByteSource.{ ByteSourceError, IO, Underfull }
import scala.util.{ Failure, Success, Try }

trait Parser[M] {

  def parseFrom(bytes: ByteSource)(implicit M: Message[M]): M Or DecoderError =
    messageDecoder.run(bytes)._2

  def parseFrom(bytes: ByteString)(implicit M: Message[M]): M Or DecoderError =
    parseFrom(bytes: ByteSource)

  def parseFrom(bytes: Array[Byte])(implicit M: Message[M]): M Or DecoderError =
    parseFrom(bytes: IndexedSeq[Byte])

  def parseFrom(bytes: InputStream)(implicit M: Message[M]): M Or DecoderError =
    parseFrom(new InputStreamByteSource(bytes))

  def parsePartialFrom(bytes: ByteSource)(implicit M: Message[M]): Builder[M] Or DecoderError =
    builderDecoder.run(bytes)._2

  def parsePartialFrom(bytes: ByteString)(implicit M: Message[M]): Builder[M] Or DecoderError =
    parsePartialFrom(bytes: ByteSource)

  def parsePartialFrom(bytes: Array[Byte])(implicit M: Message[M]): Builder[M] Or DecoderError =
    parsePartialFrom(bytes: IndexedSeq[Byte])

  def parsePartialFrom(bytes: InputStream)(implicit M: Message[M]): Builder[M] Or DecoderError =
    parsePartialFrom(new InputStreamByteSource(bytes))

  private def messageDecoder(implicit M: Message[M]): Decoder[M] = for {
    builder <- builderDecoder
  } yield builder.build

  private def builderDecoder(implicit M: Message[M]): Decoder[Builder[M]] = for {
    updates <- FieldType.anyFieldDecoder[M].untilEmpty
  } yield Builder[M](updates: _*)

  private class InputStreamByteSource(val is: InputStream) extends ByteSource {
    override def isEmpty: Boolean = is.available() == 0

    def getByte: (ByteSource, Byte Or ByteSourceError) = this -> (for {
      in <- safe(is.read())
      b <- readByte(in)
    } yield b)

    private def readByte(in: Int): Byte Or ByteSourceError = in match {
      case -1 => Bad(Underfull(1, 0))
      case b => Good(b.toByte)
    }

    def getBytes(n: Int): (ByteSource, IndexedSeq[Byte] Or ByteSourceError) = {
      val buffer = new Array[Byte](n)

      def loop(read: Int): Unit Or ByteSourceError = for {
        c <- safe(is.read(buffer, read, buffer.length - read))
        next <- c match {
          case -1 => Bad(Underfull(n, read))
          case _ if c + read == buffer.length => Good(())
          case _ => loop(c + read)
        }
      } yield next

      (this, loop(0) map (_ => buffer: IndexedSeq[Byte]))
    }

    private def safe[T](io: => T): T Or ByteSourceError = Try(io) match {
      case Failure(e: IOException) => Bad(IO(e))
      case Success(x) => Good(x)
      case Failure(e) => throw e
    }
  }

}
