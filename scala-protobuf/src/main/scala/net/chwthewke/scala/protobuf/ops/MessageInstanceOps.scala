package net.chwthewke.scala.protobuf.ops

import java.io.InputStream
import net.chwthewke.scala.protobuf.{Decoder, ByteSource, Parser, Message}
import net.chwthewke.scala.protobuf.akka.util.ByteString
import net.chwthewke.scala.protobuf.scalautils.Or
import scala.language.implicitConversions

trait MessageInstanceOps[M] {
  implicit def self: Message[M]

  def parser: Parser[M] = Parser[M]

  def parse(in: InputStream): M Or Decoder.Error = parser.parseFrom(in)
  def parse(bytes: Array[Byte]): M Or Decoder.Error = parser.parseFrom(bytes)
  def parse(bytes: ByteString): M Or Decoder.Error = parser.parseFrom(bytes)
  def parse(source: ByteSource): M Or Decoder.Error = parser.parseFrom(source)
}

trait ToMessageInstanceOps {
  implicit def toMessageInstanceOps[M](message: Message[M]): MessageInstanceOps[M] =
    new MessageInstanceOps[M] {def self: Message[M] = message}
}

object messageInstance extends ToMessageInstanceOps