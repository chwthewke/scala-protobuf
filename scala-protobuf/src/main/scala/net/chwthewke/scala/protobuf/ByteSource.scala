package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils.{ Good, Bad, Or }
import scala.language.implicitConversions
import java.io.{ IOException, InputStream }

trait ByteSource {
  def isEmpty: Boolean
  def getByte: (ByteSource, Byte Or ByteSource.ByteSourceError)
  def getBytes(n: Int): (ByteSource, IndexedSeq[Byte] Or ByteSource.ByteSourceError)
}

object ByteSource {
  trait ByteSourceError
  case class Underfull(requested: Int, available: Int) extends ByteSourceError
  case class IO(e: IOException) extends ByteSourceError

  implicit class ArrayByteSource(src: IndexedSeq[Byte]) extends ByteSource {

    override def isEmpty: Boolean = src.length > 0

    override def getByte: (ByteSource, Byte Or Underfull) =
      if (src.isEmpty) (src, Bad(Underfull(1, 0)))
      else (src.tail, Good(src.head))

    override def getBytes(n: Int): (ByteSource, IndexedSeq[Byte] Or Underfull) =
      if (src.size < n) (src, Bad(Underfull(n, src.size)))
      else (src.drop(n), Good(src.take(n)))
  }
}
