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

    val fieldArgs: Seq[Tree] = Seq[Tree](LIT(field.name), LIT(field.number)) ++
      (if (field.label == LABEL_REPEATED) Seq[Tree](LIT(field.packed)) else Nil) ++
      Seq[Tree](fieldType(field), WILDCARD DOT fieldSymbol.defn)

    VAL(fieldSymbol.defn) := (fieldTypeModule
      APPLYTYPE (fieldSymbol.componentType, symbol.cls)
      APPLY fieldArgs)
  }

  def fieldType(field: FieldDescriptorProto): Tree = {
    val fieldTypeClass = FieldTypeObject DOT fieldTypeClassNames(field.typ)

    lazy val fieldSymbol: FieldSymbol = symbolTable.field(field).get

    def enumObject = for {
      enumDesc <- fieldSymbol.componentRef match {
        case EnumRef(e) => Some(e)
        case _ => None
      }
      enumSymbol <- symbolTable.enum(enumDesc)
    } yield REF(enumSymbol.cls)

    field.typ match {
      case TYPE_MESSAGE | TYPE_GROUP => (NEW(fieldTypeClass)
        APPLYTYPE (fieldSymbol.componentType))
      case TYPE_ENUM => NEW(fieldTypeClass) APPLY (enumObject.get)
      case _ => fieldTypeClass
    }
  }

  val fieldTypeClassNames: Map[FieldDescriptorProto.Type, String] = Map(
    TYPE_DOUBLE -> "Double",
    TYPE_FLOAT -> "Float",
    TYPE_INT64 -> "Int64",
    TYPE_UINT64 -> "UInt64",
    TYPE_INT32 -> "Int32",
    TYPE_FIXED64 -> "Fixed64",
    TYPE_FIXED32 -> "Fixed32",
    TYPE_BOOL -> "Bool",
    TYPE_STRING -> "String",
    TYPE_GROUP -> "Group",
    TYPE_MESSAGE -> "Message",
    TYPE_BYTES -> "Bytes",
    TYPE_UINT32 -> "UInt32",
    TYPE_ENUM -> "Enum",
    TYPE_SFIXED32 -> "SFixed32",
    TYPE_SFIXED64 -> "SFixed64",
    TYPE_SINT32 -> "SInt32",
    TYPE_SINT64 -> "SInt64")

  private def fieldRef(field: FieldDescriptorProto): Tree =
    REF(symbolTable.field(field).get.defn)

  private def builderEval(field: FieldDescriptorProto): Tree =
    (REF(BuilderParamName) DOT "eval")(fieldRef(field))

  private def messageTypeClassInstance: Process[ValDef] = process {
    val instanceType = appliedType(MessageTrait, List[Type](symbol.cls))
    VAL(MessageInstanceName, instanceType) withFlags (Flags.IMPLICIT) := NEW(ANONDEF(instanceType) := BLOCK(
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
    } yield (symbol.obj DOT enumSymbol.cls DOT value._2.module)

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
  private[DescriptorProcess] val MessageInstanceName = "messageInstance"

  def apply(desc: DescriptorProto, sym: ProtoSymbolTable) =
    new DescriptorProcess {
      def self = desc
      def symbolTable = sym
    }.apply

}
