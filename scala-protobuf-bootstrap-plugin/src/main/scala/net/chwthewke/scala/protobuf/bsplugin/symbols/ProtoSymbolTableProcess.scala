package net.chwthewke.scala.protobuf.bsplugin.symbols

import net.chwthewke.scala.protobuf.bsplugin.MessageContainer
import net.chwthewke.scala.protobuf.bsplugin.Process
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import treehugger.forest._
import treehugger.forest.definitions._
import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest

trait ProtoSymbolTableProcess {

  def apply(): Process[ProtoSymbolTable] = {
    def symbols(req: CodeGeneratorRequest): Vector[ProtoSymbol] = for {
      file <- req.protoFileList
      symbol <- fileSymbol(file)
    } yield symbol

    def symbolTable(req: CodeGeneratorRequest): ProtoSymbolTable =
      ComputeFieldTypes(ProtoSymbolTable(symbols(req)))

    for {
      syms <- Process.ask.map(symbolTable) :++>> (_.symbols.map(s => s"PST: ${s.mkString}"))
    } yield syms
  }

  def fileSymbol(descriptor: FileDescriptorProto): Vector[ProtoSymbol] = {
    val pkg = RootClass.newModuleClass(descriptor.javaPackage)
    val obj = pkg.newModuleClass(descriptor.javaOuterClassName)

    val ctx = Ctx(descriptor, Vector(), obj, descriptor.pkg)

    FileSymbol(
      descriptor,
      None,
      descriptor.pkg,
      descriptor,
      obj,
      pkg) +:
      contentSymbols(ctx, descriptor)
  }

  def messageSymbol(ctx: Ctx, descriptor: DescriptorProto): Vector[ProtoSymbol] = {

    val name = Names.message(descriptor.name)
    val obj = ctx.cont.newModuleClass(name)
    val cls = ctx.cont.newClass(name)
    val fqn = s"${ctx.pfqn}.${descriptor.name}"

    MessageSymbol(
      ctx.src,
      ctx.location,
      fqn,
      descriptor,
      cls,
      obj) +:
      (contentSymbols(ctx.copy(cont = obj, pfqn = fqn), descriptor) ++
        fieldSymbols(ctx.copy(cont = cls, pfqn = fqn), descriptor))
  }

  def enumSymbol(ctx: Ctx, descriptor: EnumDescriptorProto): EnumSymbol = {

    val name = Names.message(descriptor.name)
    val values = descriptor.valueList.map { v =>
      v -> ctx.cont.newModuleClass(Names.enumValue(v.name))
    }

    EnumSymbol(
      ctx.src,
      ctx.location,
      s"${ctx.pfqn}.${descriptor.name}",
      descriptor,
      ctx.cont.newClass(name),
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

  def fieldSymbols(ctx: Ctx, message: DescriptorProto): Vector[RawFieldSymbol] = {
    message.fieldList.zipWithIndex.map {
      case (f, i) =>
        fieldSymbol(ctx.copy(path = ctx.path :+ DescriptorProto.FIELD_FIELD_NUMBER :+ i), f)
    }
  }

  def fieldSymbol(ctx: Ctx, field: FieldDescriptorProto): RawFieldSymbol = {
    val name = Names.field(field.name)
    val sym: TermName = name

    RawFieldSymbol(
      ctx.src,
      ctx.location,
      s"${ctx.pfqn}.${field.name}",
      field,
      sym)
  }

  private case class Ctx(src: FileDescriptorProto, path: Vector[Int], cont: ClassSymbol, pfqn: String) {
    def location = for {
      info <- src.sourceCodeInfo
      location <- info.findLocation(path)
    } yield location
  }
}

object ProtoSymbolTableProcess extends ProtoSymbolTableProcess

