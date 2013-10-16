package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.DescriptorProtos.EnumDescriptorProto

sealed trait ProtoRef

case class MessageRef(descriptor: DescriptorProto) extends ProtoRef

case class EnumRef(descriptor: EnumDescriptorProto) extends ProtoRef

case object NoProtoRef extends ProtoRef
