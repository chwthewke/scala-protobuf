package net.chwthewke.scala.protobuf.ref

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import net.chwthewke.scala.protobuf.Process
import net.chwthewke.scala.protobuf.symbols.ProtoSymbolTable

trait TypeReference {

  def symbolTable: ProtoSymbolTable

  def apply(typename: String, refererPath: String, refererFile: FileDescriptorProto) = {

    pathSearchSpace(typename, refererPath.split('.').toVector)

    ???

  }

  private def accessibleFiles(referer: FileDescriptorProto): Process[Set[FileDescriptorProto]] = {
    for {
      req <- Process.ask
    } yield ???
  }

  private def pathSearchSpace(typename: String, refererPath: Vector[String]): List[String] =
    if (typename(0) == '.') List(typename.drop(1))
    else refererPath.tails
      .map(pre => s"${pre.mkString(".")}.$typename")
      .toList

}
