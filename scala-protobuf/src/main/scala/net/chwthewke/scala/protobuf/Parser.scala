package net.chwthewke.scala.protobuf

import java.io.{ IOException, InputStream }
import net.chwthewke.scala.protobuf.scalautils.{ Good, Bad, Or }
import net.chwthewke.scala.protobuf.ByteSource.{ IO, Underfull }
import scala.util.{ Failure, Success, Try }

trait Parser[M] extends ProtobufDecoder[M] {

  def parseFrom(bytes: ByteSource): M Or Decoder.Error =
    decoder.run(bytes)._2

  def parseFrom(bytes: ByteString): M Or Decoder.Error =
    parseFrom(bytes: ByteSource)

  def parseFrom(bytes: Array[Byte]): M Or Decoder.Error =
    parseFrom(bytes: IndexedSeq[Byte])

  def parseFrom(bytes: InputStream): M Or Decoder.Error =
    parseFrom(new InputStreamByteSource(bytes))

  def parsePartialFrom(bytes: ByteSource): M.Builder Or Decoder.Error =
    builderDecoder.run(bytes)._2

  def parsePartialFrom(bytes: ByteString): M.Builder Or Decoder.Error =
    parsePartialFrom(bytes: ByteSource)

  def parsePartialFrom(bytes: Array[Byte]): M.Builder Or Decoder.Error =
    parsePartialFrom(bytes: IndexedSeq[Byte])

  def parsePartialFrom(bytes: InputStream): M.Builder Or Decoder.Error =
    parsePartialFrom(new InputStreamByteSource(bytes))

  private class InputStreamByteSource(val is: InputStream) extends ByteSource {
    override def isEmpty: Boolean = is.available() == 0

    def getByte: (ByteSource, Byte Or ByteSource.Error) = this -> (for {
      in <- safe(is.read())
      b <- readByte(in)
    } yield b)

    private def readByte(in: Int): Byte Or ByteSource.Error = in match {
      case -1 => Bad(Underfull(1, 0))
      case b  => Good(b.toByte)
    }

    def getBytes(n: Int): (ByteSource, IndexedSeq[Byte] Or ByteSource.Error) = {
      val buffer = new Array[Byte](n)

      def loop(read: Int): Unit Or ByteSource.Error = for {
        c <- safe(is.read(buffer, read, buffer.length - read))
        next <- c match {
          case -1                             => Bad(Underfull(n, read))
          case _ if c + read == buffer.length => Good(())
          case _                              => loop(c + read)
        }
      } yield next

      (this, loop(0) map (_ => buffer: IndexedSeq[Byte]))
    }

    private def safe[T](io: => T): T Or ByteSource.Error = Try(io) match {
      case Failure(e: IOException) => Bad(IO(e))
      case Success(x)              => Good(x)
      case Failure(e)              => throw e
    }
  }

}

object Parser {
  def apply[M](implicit M0: Message[M]): Parser[M] = new Parser[M] { val M = M0 }
}
