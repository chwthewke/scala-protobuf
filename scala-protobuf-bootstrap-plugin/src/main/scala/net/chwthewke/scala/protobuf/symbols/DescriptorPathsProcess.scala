package net.chwthewke.scala.protobuf.symbols

import net.chwthewke.scala.protobuf
import net.chwthewke.scala.protobuf.Process
import com.google.protobuf.DescriptorProtos.{ DescriptorProto, EnumDescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf.MessageContainer
import net.chwthewke.scala.protobuf.PluginOps
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait DescriptorPathsProcess {

  import PluginOps._
  import MessageContainer._
  import DescriptorPathsProcess._
  import protobuf._

  def apply(): Process[(Map[DescriptorPath, DescriptorProto], Map[DescriptorPath, EnumDescriptorProto])] = {
    for {
      req <- Process.ask
      fileDescriptorPaths <- req.protoFileList.map(descriptorPaths).sequence
      (messages, enums) = fileDescriptorPaths.unzip
    } yield (messages.flatten.toMap, enums.flatten.toMap)
  }

  private def descriptorPaths(file: FileDescriptorProto): Process[CombinedDescriptorPaths] =
    nestedDescriptorPaths(DescriptorPath(file), file)

  private def nestedDescriptorPaths[A: MessageContainer](root: DescriptorPath, mc: A): Process[CombinedDescriptorPaths] = {
    for {
      nested <- mc.messages.map(descriptorPaths(root, _)).sequence
      (nestedMessages, nestedEnums) = nested.unzip
      enums <- enumDescriptorPaths(root, mc)
    } yield (nestedMessages.flatten, nestedEnums.flatten ++ enums)

  }

  private def descriptorPaths(parent: DescriptorPath, descriptor: DescriptorProto): Process[CombinedDescriptorPaths] = {
    for {
      ownPath <- Process(parent + descriptor.name) :+>> (p => s"DP:  ${p.mkString}")
      nested <- nestedDescriptorPaths(ownPath, descriptor)
      (nestedMessages, nestedEnums) = nested
    } yield ((ownPath -> descriptor) +: nestedMessages, nestedEnums)
  }

  private def enumDescriptorPaths[A: MessageContainer](parent: DescriptorPath, mc: A): Process[EnumDescriptorPaths] =
    mc.enums.map(enumDescriptorPath(parent, _)).sequence

  private def enumDescriptorPath(parent: DescriptorPath, enum: EnumDescriptorProto): Process[(DescriptorPath, EnumDescriptorProto)] =
    for (e <- Process(parent + enum.name) :+>> (e => s"DP:  ${e.mkString}"))
      yield e -> enum

}

object DescriptorPathsProcess extends DescriptorPathsProcess {
  type DescriptorPaths = Vector[(DescriptorPath, DescriptorProto)]
  type EnumDescriptorPaths = Vector[(DescriptorPath, EnumDescriptorProto)]
  type CombinedDescriptorPaths = (DescriptorPaths, EnumDescriptorPaths)
}