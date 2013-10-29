package net.chwthewke.scala.protobuf

import scala.annotation.tailrec
import scala.language.implicitConversions
import scalautils.Or
import scalautils.Good
import scalautils.Bad

trait Decoder[+A] {
  import Decoder._

  def run(source: ByteSource): (ByteSource, A Or DecoderError)

  def map[B](f: A => B): Decoder[B] = Decoder[B] {
    run(_) match {
      case (source, a) => (source, a.map(f))
    }
  }

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def run(source: ByteSource) = Decoder.this.run(source) match {
      case (newSource, Good(a)) => f(a).run(newSource)
      case (newSource, Bad(e)) => (newSource, Bad(e))
    }
  }

  def filter(p: A => Boolean): Decoder[A] = new Decoder[A] {
    def run(source: ByteSource) = Decoder.this.run(source) match {
      case (newSource, Good(a)) if p(a) => (newSource, Good(a))
      case (newSource, Good(a)) => (newSource, Bad(MalformedProtobuf))
    }
  }

  def withFilter(p: A => Boolean): Decoder[A] = filter(p)

  def badMap(f: DecoderError => DecoderError): Decoder[A] = new Decoder[A] {
    def run(source: ByteSource) = Decoder.this.run(source) match {
      case (newSource, Bad(err)) => (newSource, Bad(f(err)))
      case any => any
    }
  }

  def untilEmpty: Decoder[Vector[A]] = new Decoder[Vector[A]] {
    @tailrec
    def loop(source: ByteSource, acc: Vector[A]): (ByteSource, Vector[A] Or DecoderError) =
      if (source.isEmpty) (source, Good(acc))
      else Decoder.this.run(source) match {
        case (src, Good(a)) => loop(src, acc :+ a)
        case (src, Bad(e)) => (src, Bad(e))
      }

    def run(source: ByteSource): (ByteSource, Or[Vector[A], DecoderError]) = loop(source, Vector.empty)
  }

}

object Decoder {
  def apply[A](r: ByteSource => (ByteSource, A Or DecoderError)) = new Decoder[A] {
    override def run(source: ByteSource): (ByteSource, A Or DecoderError) = r(source)
  }

  def constant[A](r: => A Or DecoderError) = Decoder[A]((_, r))

  trait Chainable {
    def self: Decoder[ByteSource]
    def into[M](sink: Decoder[M]): Decoder[M] = self.flatMap(chained => constant(sink.run(chained)._2))
  }

  implicit def ToChainable(d: Decoder[ByteSource]): Chainable = new Chainable { def self = d }

  sealed trait DecoderError

  case object TruncatedProtobuf extends DecoderError
  case object MalformedProtobuf extends DecoderError
  case object TooLongVarInt extends DecoderError
  case object TruncatedVarInt extends DecoderError
  case object VarInt32Overflow extends DecoderError
  case object InvalidEnumValue extends DecoderError
  case object InvalidUtf8 extends DecoderError
  case object WireTypeMismatch extends DecoderError
}

