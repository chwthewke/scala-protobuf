package net.chwthewke.scala.protobuf

import scala.collection.immutable.Seq
import scala.language.higherKinds

trait Encoder[-A] {

  self =>

  def run(in: A): Stream[Byte]

  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    override def run(in: B) = self.run(f(in))
  }

  def repeated: Encoder[Seq[A]] = new Encoder[Seq[A]] {
    override def run(in: Seq[A]) = in.toStream.flatMap(self.run)
  }

  def ++[B <: A](other: Encoder[B]): Encoder[B] = new Encoder[B] {
    override def run(in: B) = self.run(in) #::: other.run(in)
  }

  def from(a: => A): Encoder[Any] = new Encoder[Any] {
    override def run(in: Any) = self.run(a)
  }

  def into[B](sink: Encoder[B])(f: Stream[Byte] => B): Encoder[A] = new Encoder[A] {
    override def run(in: A) = sink.run(f(self.run(in)))
  }

}

object Encoder {
  def constant(r: => Stream[Byte]): Encoder[Any] = new Encoder[Any] {
    override def run(in: Any): Stream[Byte] = r
  }

  def sequence[A, CC[X] <: Iterable[X]](encoders: CC[Encoder[A]]): Encoder[A] =
    (encoders :\ (constant(Stream.empty): Encoder[A]))(_ ++ _)

}

