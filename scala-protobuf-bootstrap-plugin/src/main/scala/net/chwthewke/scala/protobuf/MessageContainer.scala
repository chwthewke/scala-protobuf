package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import scalaz.syntax.Ops

trait MessageContainer[A] {
  def messages(a: A): Vector[DescriptorProto]

  def sourceName(a: A): String

  def codeName(a: A): String
}

object MessageContainer {
  import PluginOps._

  implicit val FileDescriptorInstance: MessageContainer[FileDescriptorProto] =
    new MessageContainer[FileDescriptorProto] {
      def messages(file: FileDescriptorProto) = file.messageTypeList

      def sourceName(file: FileDescriptorProto): String = file.name

      def codeName(file: FileDescriptorProto): String = file.javaOuterClassName
    }

  implicit val MessageDescriptorInstance: MessageContainer[DescriptorProto] =
    new MessageContainer[DescriptorProto] {
      def messages(descriptor: DescriptorProto) = descriptor.nestedTypeList

      def sourceName(descriptor: DescriptorProto): String = descriptor.name

      def codeName(descriptor: DescriptorProto): String = descriptor.name
    }

  implicit class MessageContainerOps[A](override val self: A)(implicit MC: MessageContainer[A]) extends Ops[A] {
    def messages = MC.messages(self)

    def sourceName: String = MC.sourceName(self)

    def codeName: String = MC.codeName(self)
  }

  def apply[A](implicit MC: MessageContainer[A]): MessageContainer[A] = MC
}
