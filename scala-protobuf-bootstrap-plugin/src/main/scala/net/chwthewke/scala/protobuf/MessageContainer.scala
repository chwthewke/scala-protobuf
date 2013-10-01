package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, FileDescriptorProto }
import scalaz.syntax.Ops

trait MessageContainer[A] {
  def messages(a: A): Vector[DescriptorProto]

  def enums(a: A): Vector[EnumDescriptorProto]

  // TODO get these from the actual field numbers ?
  def messageType: Int

  def enumType: Int
}

object MessageContainer {
  import PluginOps._

  implicit val FileDescriptorInstance: MessageContainer[FileDescriptorProto] =
    new MessageContainer[FileDescriptorProto] {
      def messages(file: FileDescriptorProto) = file.messageTypeList

      def enums(file: FileDescriptorProto) = file.enumTypeList

      def messageType = 4

      def enumType = 5
    }

  implicit val MessageDescriptorInstance: MessageContainer[DescriptorProto] =
    new MessageContainer[DescriptorProto] {
      def messages(descriptor: DescriptorProto) = descriptor.nestedTypeList

      def enums(descriptor: DescriptorProto) = descriptor.enumTypeList

      def messageType = 3

      def enumType = 4
    }

  implicit class MessageContainerOps[A](override val self: A)(implicit MC: MessageContainer[A]) extends Ops[A] {
    def messages = MC.messages(self)

    def enums = MC.enums(self)

    def messageType = MC.messageType

    def enumType = MC.enumType
  }

  def apply[A](implicit MC: MessageContainer[A]): MessageContainer[A] = MC
}
