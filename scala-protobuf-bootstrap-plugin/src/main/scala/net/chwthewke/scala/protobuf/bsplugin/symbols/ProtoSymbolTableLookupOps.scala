package net.chwthewke.scala.protobuf.bsplugin.symbols

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{ Type => FType }
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
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
      case es @ EnumSymbol(_, _, _, e, _, _, _) if descriptor == e => es
    }

  def field(descriptor: FieldDescriptorProto): Option[FieldSymbol] =
    self.symbols.collectFirst {
      case fs @ FieldSymbol(_, _, _, f, _) if descriptor == f => fs
    }

  def findByName(typename: String, referrerFqn: String, referrerSource: FileDescriptorProto): Option[ProtoSymbol] = {

    val reachableFiles = self.symbols.collect {
      case fs: FileSymbol if referrerSource.dependencyList.contains(fs.descriptor.name) => fs.descriptor
    }

    (none[ProtoSymbol] /: fqns(typename, referrerFqn)) {
      case (Some(s), _) => Some(s)
      case (_, fqn)     => findByFqn(fqn, referrerSource +: reachableFiles)
    }
  }

  private def findByFqn(fqn: String, reachableFiles: Seq[FileDescriptorProto]): Option[ProtoSymbol] = {
    self.symbols.collectFirst { case ps: ProtoSymbol if ps.fqn == fqn && reachableFiles.contains(ps.file) => ps }
  }

  private def fqns(typename: String, referrerFqn: String): Vector[String] =
    if (typename(0) == '.') Vector(typename.drop(1))
    else for (init <- referrerFqn.split('.').inits.toVector) yield (init :+ typename).mkString(".")

  // TODO hackish
  def typeRef(fieldSymbol: FieldSymbol): ProtoRef = {
    val fieldDesc = fieldSymbol.descriptor
    val fileDesc = fieldSymbol.file
    val fieldType = fieldDesc.typ
    val typeName = fieldDesc.typeName
    val fqn = fieldSymbol.fqn
    ((simpleTypeSymbol andThen PrimitiveRef.apply) orElse referenceTypeSymbol(fqn, fileDesc))
      .lift(fieldType, typeName).get
  }

  private def referenceTypeSymbol(fqn: String, file: FileDescriptorProto): PartialFunction[(FType, Option[String]), ProtoRef] = {
    case (TYPE_ENUM, Some(typeName))    => enum(typeName, fqn, file)
    case (TYPE_GROUP, Some(typeName))   => message(typeName, fqn, file)
    case (TYPE_MESSAGE, Some(typeName)) => message(typeName, fqn, file)
  }

  private def simpleTypeSymbol: PartialFunction[(FType, Option[String]), String] = {
    // TODO case class
    case (TYPE_BOOL, _)   => "Boolean"
    case (TYPE_BYTES, _)  => "net.chwthewke.scala.protobuf.ByteString"
    case (TYPE_DOUBLE, _) => "Double"
    case (TYPE_FIXED32, _) | (TYPE_INT32, _) |
      (TYPE_SFIXED32, _) | (TYPE_SINT32, _) |
      (TYPE_UINT32, _) => "Int"
    case (TYPE_FIXED64, _) | (TYPE_INT64, _) |
      (TYPE_SFIXED64, _) | (TYPE_SINT64, _) |
      (TYPE_UINT64, _) => "Long"
    case (TYPE_FLOAT, _)  => "Float"
    case (TYPE_STRING, _) => "String"
  }

  def enum(typeName: String, fqn: String, file: FileDescriptorProto): ProtoRef =
    self.findByName(typeName, fqn, file).collect {
      case EnumSymbol(_, _, _, _, _, javaFqn, _) => EnumRef(javaFqn)
    }.get

  def message(typeName: String, fqn: String, file: FileDescriptorProto): ProtoRef =
    self.findByName(typeName, fqn, file).collect {
      case MessageSymbol(_, _, _, desc, _, javaFqn) => MessageRef(javaFqn)
    }.get

}
