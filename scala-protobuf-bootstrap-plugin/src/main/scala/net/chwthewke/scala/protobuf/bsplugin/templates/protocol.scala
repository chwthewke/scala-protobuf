package net.chwthewke.scala.protobuf.bsplugin.templates

import scala.language.postfixOps

trait Content {
  def all: String
}

/**
 * - className: enum class name (not FQ)
 * - name: upper-case name
 * - number: as per proto
 */
case class EnumValueDef(name: String, number: Int) extends Format {
  def ref = name

  def enumValueDef(className: String) =
    s"""case object $name extends $className("$name", $number)"""
}

case class EnumDef(name: String, values: Seq[EnumValueDef]) extends Format with Content {

  def enumClass = s"sealed abstract class $name(val name: String, val number: Int) extends $name.Value"

  def enumObject = s"""object $name extends ${names.protobufEnum}[$name] {
                      | 
                      |${values map (_.enumValueDef(name)) mkDefList}
                      | 
                      |  val values: Vector[$name] = Vector(
                      |${indent(1, values map (_.name) mkArgList)})
                      |
                      |  override def name(value: $name): String = value.name 
                      |  override def number(value: $name): Int = value.number 
                      |
                      |  implicit val protobufEnumInstance: ${names.protobufEnum}[$name] = this
                      |}
                      |""".stripMargin

  def all = enumClass + "\n\n" + enumObject

}

/**
 * - name: as per proto
 * - number: as per proto
 * - ctor: local ref (Message member)
 * - compType: FQN
 * - compMult: Some("Vector"), Some("Option") or None
 * - fieldType: FQN of net.chwthewke.scala.protobuf.FieldType instance
 */
case class FieldDef(
  name: String, number: Int, ctor: String,
  compType: String, compMult: Option[String], fieldType: String) extends Format {

  def field = s"""val `$name`: $ctor[$compType] = 
                 |  $ctor[$compType]("$name", $number, $fieldType, _.`$name`)
                 |""".stripMargin

  def ref = s"${names.fields}.`$name`"

  def builderArg = s"${names.builderParam}.eval($ref)"

  def classParam = s"`$name`: $classParamType$classParamDefault"

  def classParamType = compMult match {
    case Some("Vector") => s"Vector[$compType]"
    case Some("Option") => s"Option[$compType]"
    case _              => compType
  }

  def classParamDefault = compMult match {
    case Some("Vector") => " = Vector()"
    case Some("Option") => " = None"
    case _              => ""
  }

}

case class FieldDefs(fieldDefs: Seq[FieldDef]) extends Format {

  def fields = s"""object ${names.fields} {
                  |
                  |${fieldDefs map (_.field) mkDefList}
                  |}""".stripMargin

  def fieldVector = s"""lazy val fields: Vector[${names.field}] = Vector(
                       |${fieldDefs map (_.ref) mkArgList})""".stripMargin

  def builderParams = (fieldDefs map (_.builderArg) mkArgList)

  def classParams = (fieldDefs map (_.classParam) mkArgList)

}

case class MessageDef(name: String, fieldDefs: FieldDefs, contents: Seq[Content])
  extends Format with Content {
  def instance = s"""object $name extends ${names.message}[$name] {
                    |
                    |  implicit val messageInstance: ${names.message}[$name] = this
                    |
                    |${indent(1, fieldDefs.fields)}
                    |
                    |${indent(1, fieldDefs.fieldVector)}
                    |
                    |  def build(${names.builderParam}: $name.${names.builder}): $name = $name(
                    |${indent(1, fieldDefs.builderParams)})
                    |
                    |${contents map (_.all) mkDefList}
                    |}
                    |""".stripMargin

  // TODO ++ and builder should be implementable through an implicit conversion
  def caseClass = s"""case class $name(
                     |${fieldDefs.classParams}) {
                     |   
                     |  def builder: $name.${names.builder} = $name.${names.newBuilder}(this)
                     |
                     |  def ++(other: $name): $name = $name.merge(this, other)
                     |
                     |  def updated(mods: $name.${names.field}#${names.update}*): $name = $name.mod(this, mods)
                     |}
                     |""".stripMargin

  def all = caseClass + "\n" + instance
}

case class ProtoDef(pkg: String, name: String, contents: Seq[Content]) extends Format with Content {

  def pkgName = s"package $pkg"

  def protocolObject = s"""object $name {
                          |
                          |${contents map (_.all) mkDefList}
                          |}
                          |""".stripMargin

  def all = pkgName + "\n\n" + protocolObject

  def file = s"${pkg.replace('.', '/')}/$name.scala"

}

trait Format {
  def indent(n: Int, str: String) = ("" /: (str.linesWithSeparators map (("  " * n) + _)))(_ + _)

  implicit class mkList(strings: Seq[String]) {
    def mkArgList = mkLines(1, ",")
    def mkDefList = mkLines(1, "")

    def mkLines(t: Int, sep: String): String =
      indent(t, strings mkString (sep + "\n"))
  }
}

object names {
  val pkg = "net.chwthewke.scala.protobuf"
  val message = s"$pkg.Message"
  val protobufEnum = s"$pkg.ProtobufEnum"
  val builder = "Builder"
  val fields = "Fields"
  val field = "Field"
  val update = "Update"
  val newBuilder = "newBuilder"
  val builderParam = "b"
}
