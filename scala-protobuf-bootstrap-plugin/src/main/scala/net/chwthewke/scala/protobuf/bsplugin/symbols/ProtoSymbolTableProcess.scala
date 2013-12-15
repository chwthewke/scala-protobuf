package net.chwthewke.scala.protobuf.bsplugin.symbols

import net.chwthewke.scala.protobuf.bsplugin.MessageContainer
import net.chwthewke.scala.protobuf.bsplugin.Process
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.SourceCodeInfo.Location
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest

trait ProtoSymbolTableProcess {

  def apply(): Process[ProtoSymbolTable] = {
    def symbols(req: CodeGeneratorRequest): Vector[ProtoSymbol] = for {
      file <- req.protoFileList
      symbol <- fileSymbol(file)
    } yield symbol

    def symbolTable(req: CodeGeneratorRequest): ProtoSymbolTable =
      ProtoSymbolTable(symbols(req))

    for {
      syms <- Process.ask.map(symbolTable) :++>> (_.symbols.map(s => s"PST: ${s.mkString}"))
    } yield syms
  }

  def fileSymbol(descriptor: FileDescriptorProto): Vector[ProtoSymbol] = {
    val ctx = Ctx(descriptor, Vector(), descriptor.pkg,
      s"${descriptor.javaPackage}.${descriptor.javaOuterClassName}")

    FileSymbol(
      descriptor,
      None,
      descriptor.pkg,
      descriptor,
      descriptor.javaOuterClassName,
      descriptor.javaPackage) +:
      contentSymbols(ctx, descriptor)
  }

  def messageSymbol(ctx: Ctx, descriptor: DescriptorProto): Vector[ProtoSymbol] = {

    val name = Names.message(descriptor.name)
    val fqn = s"${ctx.pfqn}.${descriptor.name}"
    val jfqn = s"${ctx.jfqn}.${descriptor.name}"

    MessageSymbol(
      ctx.src,
      ctx.location,
      fqn,
      descriptor,
      name,
      jfqn) +:
      (contentSymbols(ctx.copy(pfqn = fqn, jfqn = jfqn), descriptor) ++
        fieldSymbols(ctx.copy(pfqn = fqn, jfqn = jfqn), descriptor))
  }

  def enumSymbol(ctx: Ctx, descriptor: EnumDescriptorProto): EnumSymbol = {

    val name = Names.message(descriptor.name)
    val values = descriptor.valueList.map { v =>
      v -> Names.enumValue(v.name)
    }

    val jfqn = s"${ctx.jfqn}.${descriptor.name}"

    EnumSymbol(
      ctx.src,
      ctx.location,
      s"${ctx.pfqn}.${descriptor.name}",
      descriptor,
      name,
      jfqn,
      values.toMap)
  }

  def contentSymbols[MC: MessageContainer](ctx: Ctx, mc: MC): Vector[ProtoSymbol] = {
    val messageSymbols: Vector[ProtoSymbol] = mc.messages.zipWithIndex.flatMap {
      case (m, i) =>
        messageSymbol(ctx.copy(path = ctx.path :+ mc.messageType :+ i), m)
    }

    val enumSymbols: Vector[EnumSymbol] = mc.enums.zipWithIndex.map {
      case (e, i) =>
        enumSymbol(ctx.copy(path = ctx.path :+ mc.enumType :+ i), e)
    }

    messageSymbols ++ enumSymbols
  }

  def fieldSymbols(ctx: Ctx, message: DescriptorProto): Vector[FieldSymbol] = {
    message.fieldList.zipWithIndex.map {
      case (f, i) =>
        fieldSymbol(ctx.copy(path = ctx.path :+ DescriptorProto.FIELD_FIELD_NUMBER :+ i), f)
    }
  }

  def fieldSymbol(ctx: Ctx, field: FieldDescriptorProto): FieldSymbol = {
    val name = Names.field(field.name)

    FieldSymbol(
      ctx.src,
      ctx.location,
      s"${ctx.pfqn}.${field.name}",
      field,
      name)
  }

  private case class Ctx(src: FileDescriptorProto, path: Vector[Int], pfqn: String, jfqn: String) {
    def location = for {
      info <- src.sourceCodeInfo
      location <- info.findLocation(path)
    } yield location
  }
}

object ProtoSymbolTableProcess extends ProtoSymbolTableProcess

