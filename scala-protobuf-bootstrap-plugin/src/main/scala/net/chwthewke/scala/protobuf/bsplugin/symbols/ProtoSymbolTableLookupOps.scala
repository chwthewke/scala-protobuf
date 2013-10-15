package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scalaz.syntax.Ops
import scalaz.std.option._

trait ProtoSymbolTableLookupOps extends Ops[ProtoSymbolTable] {

  def file(descriptor: FileDescriptorProto): Option[FileSymbol] =
    self.symbols.collectFirst {
      case fs @ FileSymbol(_, _, _, f, _, _) if descriptor == f => fs
    }

  def message(descriptor: DescriptorProto): Option[MessageSymbol] =
    self.symbols.collectFirst {
      case ms @ MessageSymbol(_, _, _, m, _, _) if descriptor == m => ms
    }

  def enum(descriptor: EnumDescriptorProto): Option[EnumSymbol] =
    self.symbols.collectFirst {
      case es @ EnumSymbol(_, _, _, e, _, _) if descriptor == e => es
    }

  def field(descriptor: FieldDescriptorProto): Option[FieldSymbol] =
    self.symbols.collectFirst {
      case fs @ FieldSymbol(_, _, _, f, _, _, _) if descriptor == f => fs
    }

  def findByName(typename: String, referrerFqn: String, referrerSource: FileDescriptorProto): Option[ProtoSymbol] = {

    val reachableFiles = self.symbols.collect {
      case fs: FileSymbol if referrerSource.dependencyList.contains(fs.descriptor.name) => fs.descriptor
    }

    (none[ProtoSymbol] /: fqns(typename, referrerFqn)) {
      case (Some(s), _) => Some(s)
      case (_, fqn) => findByFqn(fqn, referrerSource +: reachableFiles)
    }
  }

  private def findByFqn(fqn: String, reachableFiles: Seq[FileDescriptorProto]): Option[ProtoSymbol] = {
    self.symbols.collectFirst { case ps: ProtoSymbol if ps.fqn == fqn && reachableFiles.contains(ps.file) => ps }
  }

  private def fqns(typename: String, referrerFqn: String): Vector[String] =
    if (typename(0) == '.') Vector(typename.drop(1))
    else for (init <- referrerFqn.split('.').inits.toVector) yield (init :+ typename).mkString(".")

}
