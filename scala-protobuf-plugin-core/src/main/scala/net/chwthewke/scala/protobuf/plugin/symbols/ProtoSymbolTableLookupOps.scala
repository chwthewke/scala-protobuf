package net.chwthewke.scala.protobuf.plugin.symbols

import net.chwthewke.scala.protobuf.plugin.interface._
import net.chwthewke.scala.protobuf.plugin.interface.field.{ Type => FType }
import net.chwthewke.scala.protobuf.plugin.interface.field._
import net.chwthewke.scala.protobuf.plugin.syntax._
import scalaz.syntax.Ops
import scalaz.std.option._

trait ProtoSymbolTableLookupOps extends Ops[ProtoSymbolTable] {

  def file(descriptor: FileDescriptor): Option[FileSymbol] =
    self.symbols.collectFirst {
      case fs @ FileSymbol(_, _, _, f, _, _) if descriptor == f => fs
    }

  def message(descriptor: Descriptor): Option[MessageSymbol] =
    self.symbols.collectFirst {
      case ms @ MessageSymbol(_, _, _, m, _, _) if descriptor == m => ms
    }

  def enum(descriptor: EnumDescriptor): Option[EnumSymbol] =
    self.symbols.collectFirst {
      case es @ EnumSymbol(_, _, _, e, _, _, _) if descriptor == e => es
    }

  def field(descriptor: FieldDescriptor): Option[FieldSymbol] =
    self.symbols.collectFirst {
      case fs @ FieldSymbol(_, _, _, f, _) if descriptor == f => fs
    }

  def findByName(typename: String, referrerFqn: String, referrerSource: FileDescriptor): Option[ProtoSymbol] = {

    val reachableFiles = self.symbols.collect {
      case fs: FileSymbol if referrerSource.dependencies.contains(fs.descriptor.name) => fs.descriptor
    }

    (none[ProtoSymbol] /: fqns(typename, referrerFqn)) {
      case (Some(s), _) => Some(s)
      case (_, fqn)     => findByFqn(fqn, referrerSource +: reachableFiles)
    }
  }

  private def findByFqn(fqn: String, reachableFiles: Seq[FileDescriptor]): Option[ProtoSymbol] = {
    self.symbols.collectFirst { case ps: ProtoSymbol if ps.fqn == fqn && reachableFiles.contains(ps.file) => ps }
  }

  private def fqns(typename: String, referrerFqn: String): Vector[String] =
    if (typename(0) == '.') Vector(typename.drop(1))
    else for (init <- referrerFqn.split('.').inits.toVector) yield (init :+ typename).mkString(".")

  def typeRef(fieldSymbol: FieldSymbol): ProtoRef = {
    val fieldDesc = fieldSymbol.descriptor
    val fieldType = fieldDesc.typ
    val typeName = fieldDesc.typeName
    def fileDesc = fieldSymbol.file
    def fqn = fieldSymbol.fqn
    (simpleTypeSymbol orElse referenceTypeSymbol(fqn, fileDesc))
      .lift(fieldType, typeName).get
  }

  private def referenceTypeSymbol(fqn: String, file: FileDescriptor): PartialFunction[(FType, Option[String]), ProtoRef] = {
    case (TYPE_ENUM, Some(typeName))    => enum(typeName, fqn, file)
    case (TYPE_GROUP, Some(typeName))   => message(typeName, fqn, file)
    case (TYPE_MESSAGE, Some(typeName)) => message(typeName, fqn, file)
  }

  private def simpleTypeSymbol: PartialFunction[(FType, Option[String]), PrimitiveRef] = {
    case (TYPE_BOOL, _)   => BoolRef
    case (TYPE_BYTES, _)  => ByteStringRef
    case (TYPE_DOUBLE, _) => DoubleRef
    case (TYPE_FIXED32, _) | (TYPE_INT32, _) |
      (TYPE_SFIXED32, _) | (TYPE_SINT32, _) |
      (TYPE_UINT32, _) => IntRef
    case (TYPE_FIXED64, _) | (TYPE_INT64, _) |
      (TYPE_SFIXED64, _) | (TYPE_SINT64, _) |
      (TYPE_UINT64, _) => LongRef
    case (TYPE_FLOAT, _)  => FloatRef
    case (TYPE_STRING, _) => StringRef
  }

  def enum(typeName: String, fqn: String, file: FileDescriptor): ProtoRef =
    self.findByName(typeName, fqn, file).collect {
      case EnumSymbol(_, _, _, _, _, javaFqn, _) => EnumRef(javaFqn)
    }.get

  def message(typeName: String, fqn: String, file: FileDescriptor): ProtoRef =
    self.findByName(typeName, fqn, file).collect {
      case MessageSymbol(_, _, _, desc, _, javaFqn) => MessageRef(javaFqn)
    }.get

}
