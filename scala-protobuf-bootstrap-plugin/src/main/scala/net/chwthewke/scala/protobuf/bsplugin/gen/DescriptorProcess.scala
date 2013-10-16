package net.chwthewke.scala.protobuf.bsplugin.gen

import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import net.chwthewke.scala.protobuf.bsplugin._
import net.chwthewke.scala.protobuf.bsplugin.symbols._
import net.chwthewke.scala.protobuf.bsplugin.syntax._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import treehugger.forest._
import treehugger.forest.definitions._
import treehugger.forest.treehuggerDSL._

trait DescriptorProcess {

  import DescriptorProcess._

  def symbolTable: ProtoSymbolTable

  def self: DescriptorProto

  lazy val symbol: MessageSymbol = symbolTable.message(self).get

  def apply: Process[Vector[Tree]] = {

    for {
      nested <- MessageContainerProcess(self, symbolTable)
      enums <- self.enumTypeList.map(EnumDescriptorProcess(_, symbolTable)).sequence
      tcInstance <- messageTypeClassInstance
      fieldInstances <- self.fieldList.map(fieldInstance).sequence
      fields <- self.fieldList.map(classVal).sequence
    } yield Vector[Tree](
      CASECLASSDEF(symbol.cls) withParams (fields) withParents (appliedType(MessageSyntaxTrait, List[Type](symbol.cls))),
      objectDef(nested ++ enums.flatten ++ fieldInstances :+ tcInstance))

  }

  private def fieldInstance(field: FieldDescriptorProto): Process[ValDef] = process {
    val fieldSymbol: FieldSymbol = symbolTable.field(field).get

    val fieldTypeModule: ModuleClassSymbol = field.label match {
      case LABEL_REQUIRED => RequiredObject
      case LABEL_OPTIONAL => OptionalObject
      case _ => RepeatedObject
    }

    VAL(fieldSymbol.defn) := (fieldTypeModule
      APPLYTYPE (fieldSymbol.componentType, symbol.cls)
      APPLY (LIT(field.name), LIT(field.number), WILDCARD DOT fieldSymbol.defn))
  }

  private def fieldRef(field: FieldDescriptorProto): Tree =
    REF(symbolTable.field(field).get.defn)

  private def builderEval(field: FieldDescriptorProto): Tree =
    (REF(BuilderParamName) DOT "eval")(fieldRef(field))

  private def messageTypeClassInstance: Process[ValDef] = process {

    val instanceType = appliedType(MessageTrait, List[Type](symbol.cls))
    VAL("messageInstance", instanceType) := NEW(ANONDEF(instanceType) := BLOCK(
      VAL("fields") withFlags (Flags.OVERRIDE) := (
        VectorClass
        APPLYTYPE appliedType(FieldTrait.typeConstructor, TYPE_REF("_"), TYPE_REF("_"), symbol.cls)
        APPLY (self.fieldList.map(fieldRef))
      ),
      (DEF("build") withFlags (Flags.OVERRIDE)
        withParams (PARAM(BuilderParamName, appliedType(BuilderClass.typeConstructor, List[Type](symbol.cls)))) :=
        symbol.obj APPLY self.fieldList.map(builderEval)
      )
    ))

  }

  private def objectDef(content: Vector[Tree]): Tree = {
    val od = OBJECTDEF(symbol.obj)
    if (content.isEmpty) od
    else od := BLOCK(content)
  }

  private def classVal(field: FieldDescriptorProto): Process[ValDef] = {
    val symbol: FieldSymbol = symbolTable.field(field).get
    val rawParam = PARAM(symbol.defn, symbol.fieldType)

    for {
      defaultValueDef <- defaultValueDef(field)
    } yield defaultValueDef.fold[ValDef](rawParam)(rawParam := _)
  }

  private def defaultValueDef(field: FieldDescriptorProto): Process[Option[Tree]] = process {
    val defaultValue = field.defaultValue

    def enumValueSymbol(v: String): Option[Tree] = for {
      fieldSymbol <- symbolTable.field(field)
      enum <- fieldSymbol.componentRef match {
        case EnumRef(e) => Some(e)
        case _ => None
      }
      enumSymbol <- symbolTable.enum(enum)
      value <- enumSymbol.values.find {
        case (evdp, _) => evdp.name == v
      }
    } yield REF(value._2.fullName)

    def explicitDefault(v: String): Option[Tree] = field.typ match {
      case TYPE_BOOL => Some(LIT(v.trim.toBoolean))
      case TYPE_ENUM => enumValueSymbol(v.trim)
      case _ => None
    }

    def implicitDefault: Option[Tree] = field.label match {
      case LABEL_OPTIONAL => Some(REF(NoneModule)) // TODO should it be SOME(fieldObj DOT "defaultForType") ?
      case LABEL_REPEATED => Some(VectorClass DOT "empty")
      case _ => None
    }

    defaultValue.flatMap(explicitDefault).map(SOME(_)).orElse(implicitDefault)
  }

}

object DescriptorProcess {
  private[DescriptorProcess] val BuilderParamName = "p"

  def apply(desc: DescriptorProto, sym: ProtoSymbolTable) =
    new DescriptorProcess {
      def self = desc
      def symbolTable = sym
    }.apply

}
