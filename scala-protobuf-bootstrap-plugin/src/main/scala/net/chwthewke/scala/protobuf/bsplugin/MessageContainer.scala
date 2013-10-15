package net.chwthewke.scala.protobuf.bsplugin

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto }

trait MessageContainer[A] {
  def messages(a: A): Vector[DescriptorProto]

  def enums(a: A): Vector[EnumDescriptorProto]

  def messageType: Int

  def enumType: Int
}

object MessageContainer {

  def apply[A](implicit MC: MessageContainer[A]): MessageContainer[A] = MC

}

