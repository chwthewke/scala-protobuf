package net.chwthewke.scala.protobuf

import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf.syntax._
import scalaz.syntax.Ops

trait MessageContainerSyntax {

  implicit val FileDescriptorInstance: MessageContainer[FileDescriptorProto] =
    new MessageContainer[FileDescriptorProto] {
      def messages(file: FileDescriptorProto) = file.messageTypeList

      def enums(file: FileDescriptorProto) = file.enumTypeList

      def messageType = FileDescriptorProto.MESSAGE_TYPE_FIELD_NUMBER

      def enumType = FileDescriptorProto.ENUM_TYPE_FIELD_NUMBER
    }

  implicit val MessageDescriptorInstance: MessageContainer[DescriptorProto] =
    new MessageContainer[DescriptorProto] {
      def messages(descriptor: DescriptorProto) = descriptor.nestedTypeList

      def enums(descriptor: DescriptorProto) = descriptor.enumTypeList

      def messageType = DescriptorProto.NESTED_TYPE_FIELD_NUMBER

      def enumType = DescriptorProto.ENUM_TYPE_FIELD_NUMBER
    }

  implicit class MessageContainerOps[A](override val self: A)(implicit MC: MessageContainer[A]) extends Ops[A] {
    def messages = MC.messages(self)

    def enums = MC.enums(self)

    def messageType = MC.messageType

    def enumType = MC.enumType
  }
}
