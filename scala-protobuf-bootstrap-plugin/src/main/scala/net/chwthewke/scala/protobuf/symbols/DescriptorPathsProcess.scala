package net.chwthewke.scala.protobuf.symbols

import net.chwthewke.scala.protobuf
import net.chwthewke.scala.protobuf.Process
import com.google.protobuf.DescriptorProtos.{ DescriptorProto, FileDescriptorProto }
import net.chwthewke.scala.protobuf.MessageContainer
import net.chwthewke.scala.protobuf.PluginOps
import scalaz.std.vector._
import scalaz.syntax.traverse._

trait DescriptorPathsProcess {

  import PluginOps._
  import MessageContainer._
  import DescriptorPathsProcess._
  import protobuf._

  def apply(): Process[Map[DescriptorPath, DescriptorProto]] = {
    for {
      req <- Process.ask
      fileDescriptorPaths <- req.protoFileList.map(descriptorPaths).sequence
    } yield fileDescriptorPaths.flatten.toMap
  }

  def descriptorPaths(file: FileDescriptorProto): Process[DescriptorPaths] =
    nestedDescriptorPaths(DescriptorPath(file), file)

  def nestedDescriptorPaths[A: MessageContainer](root: DescriptorPath, mc: A): Process[DescriptorPaths] = {
    for {
      nested <- mc.messages.map(descriptorPaths(root, _)).sequence
    } yield nested.flatten

  }

  def descriptorPaths(parent: DescriptorPath, descriptor: DescriptorProto): Process[DescriptorPaths] = {
    for {
      ownPath <- Process(parent + descriptor.name) :+>> (p => s"DP:  ${p.mkString}")
      nested <- nestedDescriptorPaths(ownPath, descriptor)
    } yield (ownPath -> descriptor) +: nested
  }

}

object DescriptorPathsProcess extends DescriptorPathsProcess {
  type DescriptorPaths = Vector[(DescriptorPath, DescriptorProto)]
}