package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.DescriptorProtos.EnumDescriptorProto

sealed trait ProtoRef {
  def javaFqn: String
}

case class MessageRef(javaFqn: String) extends ProtoRef

case class EnumRef(javaFqn: String) extends ProtoRef

case class PrimitiveRef(javaFqn: String) extends ProtoRef
