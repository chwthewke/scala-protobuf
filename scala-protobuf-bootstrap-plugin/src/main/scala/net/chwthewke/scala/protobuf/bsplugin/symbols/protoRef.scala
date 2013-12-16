package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.DescriptorProtos.EnumDescriptorProto

sealed trait ProtoRef {
  def javaFqn: String
}

case class MessageRef(javaFqn: String) extends ProtoRef

case class EnumRef(javaFqn: String) extends ProtoRef

abstract class PrimitiveRef(val javaFqn: String) extends ProtoRef

object PrimitiveRef {
  def unapply(r: PrimitiveRef): Option[String] = Some(r.javaFqn)
}

case object BoolRef extends PrimitiveRef("Boolean")
case object IntRef extends PrimitiveRef("Int")
case object LongRef extends PrimitiveRef("Long")
case object FloatRef extends PrimitiveRef("Float")
case object DoubleRef extends PrimitiveRef("Double")
case object StringRef extends PrimitiveRef("String")
case object ByteStringRef extends PrimitiveRef("net.chwthewke.scala.protobuf.ByteString")
