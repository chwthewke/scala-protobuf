package net.chwthewke.scala.protobuf.symbols

import net.chwthewke.scala.protobuf.Process
import net.chwthewke.scala.protobuf.MessageContainer
import net.chwthewke.scala.protobuf.syntax._
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
    for {
      syms <- Process.ask.map(symbols) :++>> (_.map(s => s"PST: ${s.mkString}"))
    } yield ProtoSymbolTable(syms)
  }

  def fileSymbol(descriptor: FileDescriptorProto): Vector[ProtoSymbol] = {
    val pkg = RootClass.newModuleClass(descriptor.javaPackage)
    val obj = pkg.newModuleClass(descriptor.javaOuterClassName)

    val ctx = Ctx(descriptor, Vector(), obj, descriptor.javaPackage)

    FileSymbol(
      descriptor,
      None,
      descriptor.javaPackage,
      descriptor,
      obj,
      pkg) +:
      contentSymbols(ctx, descriptor)
  }

  def messageSymbol(ctx: Ctx, descriptor: DescriptorProto): Vector[ProtoSymbol] = {

    val obj = ctx.cont.newModuleClass(descriptor.name)
    val fqn = ctx.pfqn + s".${descriptor.name}"

    MessageSymbol(
      ctx.src,
      ctx.location,
      fqn,
      descriptor,
      ctx.cont.newClass(descriptor.name),
      obj) +:
      contentSymbols(ctx.copy(cont = obj, pfqn = fqn), descriptor)
  }

  def enumSymbol(ctx: Ctx, descriptor: EnumDescriptorProto): EnumSymbol = {

    val values = descriptor.valueList.map { v =>
      v -> ctx.cont.newModuleClass(v.name)
    }

    EnumSymbol(
      ctx.src,
      ctx.location,
      ctx.pfqn + s".${descriptor.name}",
      descriptor,
      ctx.cont.newClass(descriptor.name),
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

  private case class Ctx(src: FileDescriptorProto, path: Vector[Int], cont: ModuleClassSymbol, pfqn: String) {
    def location = for {
      info <- src.sourceCodeInfo
      location <- info.findLocation(path)
    } yield location
  }
}

object ProtoSymbolTableProcess extends ProtoSymbolTableProcess

