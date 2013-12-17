package net.chwthewke.scala.protobuf.plugin

import net.chwthewke.scala.protobuf.plugin.syntax._
import net.chwthewke.scala.protobuf.plugin.interface._
import scalaz.syntax.Ops

trait MessageContainerSyntax {

  // TODO field numbers, migrate to Message.descriptor?

  implicit val FileDescriptorInstance: MessageContainer[FileDescriptor] =
    new MessageContainer[FileDescriptor] {
      def messages(file: FileDescriptor) = file.messageTypes

      def enums(file: FileDescriptor) = file.enumTypes

      def messageType = 4

      def enumType = 5
    }

  implicit val MessageDescriptorInstance: MessageContainer[Descriptor] =
    new MessageContainer[Descriptor] {
      def messages(descriptor: Descriptor) = descriptor.nestedTypes

      def enums(descriptor: Descriptor) = descriptor.enumTypes

      def messageType = 3

      def enumType = 4
    }

  implicit class MessageContainerOps[A](override val self: A)(implicit MC: MessageContainer[A]) extends Ops[A] {
    def messages = MC.messages(self)

    def enums = MC.enums(self)

    def messageType = MC.messageType

    def enumType = MC.enumType
  }
}
