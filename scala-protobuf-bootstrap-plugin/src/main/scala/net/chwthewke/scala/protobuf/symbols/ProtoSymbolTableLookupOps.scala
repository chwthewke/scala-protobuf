package net.chwthewke.scala.protobuf.symbols

import scalaz.syntax.Ops
import com.google.protobuf.DescriptorProtos._

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

}