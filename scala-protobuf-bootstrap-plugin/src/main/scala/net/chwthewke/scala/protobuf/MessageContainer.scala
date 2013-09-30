package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, FileDescriptorProto }
import scalaz.syntax.Ops

trait MessageContainer[A] {
  def messages(a: A): Vector[DescriptorProto]

  def enums(a: A): Vector[EnumDescriptorProto]

}

object MessageContainer {
  import PluginOps._

  implicit val FileDescriptorInstance: MessageContainer[FileDescriptorProto] =
    new MessageContainer[FileDescriptorProto] {
      def messages(file: FileDescriptorProto) = file.messageTypeList

      def enums(file: FileDescriptorProto) = file.enumTypeList

      def sourceName(file: FileDescriptorProto): String = file.name
    }

  implicit val MessageDescriptorInstance: MessageContainer[DescriptorProto] =
    new MessageContainer[DescriptorProto] {
      def messages(descriptor: DescriptorProto) = descriptor.nestedTypeList

      def enums(descriptor: DescriptorProto) = descriptor.enumTypeList

      def sourceName(descriptor: DescriptorProto): String = descriptor.name
    }

  implicit class MessageContainerOps[A](override val self: A)(implicit MC: MessageContainer[A]) extends Ops[A] {
    def messages = MC.messages(self)

    def enums = MC.enums(self)
  }

  def apply[A](implicit MC: MessageContainer[A]): MessageContainer[A] = MC
}
