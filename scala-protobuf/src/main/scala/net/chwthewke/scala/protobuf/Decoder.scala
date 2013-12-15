package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils._
import scala.annotation.tailrec

trait Decoder[+A] {

  self =>

  import Decoder._

  def run(source: ByteSource): (ByteSource, A Or Error)

  def name = super.toString

  def named(f: String => String): Decoder[A] = new Decoder[A] {
    override def run(in: ByteSource) = self.run(in)
    override val name = f(self.name)
  }

  override def toString = name

  def map[B](f: A => B): Decoder[B] = Decoder[B] {
    run(_) match {
      case (source, a) => (source, a.map(f))
    }
  }

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def run(source: ByteSource) = self.run(source) match {
      case (newSource, Good(a)) => f(a).run(newSource)
      case (newSource, Bad(e))  => (newSource, Bad(e))
    }
  }

  def filter(p: A => Boolean): Decoder[A] = new Decoder[A] {
    def run(source: ByteSource) = self.run(source) match {
      case (newSource, Good(a)) if p(a) => (newSource, Good(a))
      case (newSource, Good(a))         => (newSource, Bad(MalformedProtobuf))
      case bad                          => bad
    }
  }

  def withFilter(p: A => Boolean): Decoder[A] = filter(p)

  def recover[B >: A](f: Error => B Or Error): Decoder[B] = new Decoder[B] {
    def run(source: ByteSource) = Decoder.this.run(source) match {
      case (newSource, Bad(err)) => (newSource, f(err))
      case good                  => good
    }
  }

  def untilEmpty: Decoder[Vector[A]] = new Decoder[Vector[A]] {
    @tailrec
    def loop(source: ByteSource, acc: Vector[A]): (ByteSource, Vector[A] Or Error) =
      if (source.isEmpty) (source, Good(acc))
      else Decoder.this.run(source) match {
        case (src, Good(a)) => loop(src, acc :+ a)
        case (src, Bad(e))  => (src, Bad(e))
      }

    def run(source: ByteSource): (ByteSource, Or[Vector[A], Error]) = {
      loop(source, Vector.empty)
    }
  }

  def from[B](source: Decoder[B])(f: B => ByteSource): Decoder[A] = new Decoder[A] {
    override def run(in: ByteSource): (ByteSource, A Or Error) = {
      val (next, b) = source.run(in)
      (next, b.flatMap(f andThen self.run andThen (_._2)))
    }
  }

}

object Decoder {
  def apply[A](r: ByteSource => (ByteSource, A Or Error)) = new Decoder[A] {
    override def run(source: ByteSource): (ByteSource, A Or Error) = r(source)
  }

  def constant[A](r: => A Or Error) = Decoder[A]((_, r))

  def byteSource(n: Int): Decoder[ByteSource] = new Decoder[ByteSource] {
    override def run(in: ByteSource): (ByteSource, ByteSource Or Error) =
      in.getBytes(n) match {
        case (src, Good(bs)) => (src, Good(bs))
        case (src, _)        => (src, Bad(TruncatedProtobuf))
      }
  }

  sealed trait Error
  case object TruncatedProtobuf extends Error
  case object MalformedProtobuf extends Error
  case object TooLongVarInt extends Error
  case object TruncatedVarInt extends Error
  case object VarInt32Overflow extends Error
  case object InvalidEnumValue extends Error
  case object InvalidUtf8 extends Error
  case object WireTypeMismatch extends Error
}

