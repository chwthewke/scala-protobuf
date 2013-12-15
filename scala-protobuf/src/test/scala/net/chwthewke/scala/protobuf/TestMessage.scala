package net.chwthewke.scala.protobuf

case class TestMessage(optionalInt: Option[Int] = None,
  requiredString: String,
  repeatedBool: Vector[Boolean] = Vector(),
  packedBool: Vector[Boolean] = Vector()) {

  def builder: TestMessage.Builder = TestMessage.newBuilder(this)
  def ++(other: TestMessage): TestMessage = TestMessage.merge(this, other)
  def updated(mods: TestMessage.Field#Update*): TestMessage = TestMessage.mod(this, mods)
}

object TestMessage extends Message[TestMessage] {

  object Fields {
    val optionalInt = Optional[Int]("optionalInt", 1, FieldType.Int32, _.optionalInt)
    val requiredString = Required[String]("requiredString", 2, FieldType.String, _.requiredString)
    val repeatedBool = Repeated[Boolean]("repeatedBool", 3, FieldType.Bool, _.repeatedBool)
    val packedBool = Packed[Boolean]("packedBool", 4, FieldType.Bool, _.packedBool)
  }

  implicit val messageInstance: Message[TestMessage] = this

  def fields: Vector[Field] =
    Vector(Fields.optionalInt, Fields.requiredString, Fields.repeatedBool, Fields.packedBool)

  def build(p: TestMessage.Builder): TestMessage =
    TestMessage(
      p.eval(Fields.optionalInt),
      p.eval(Fields.requiredString),
      p.eval(Fields.repeatedBool),
      p.eval(Fields.packedBool))

  sealed abstract class CType(val name: String, val number: Int) extends CType.Value

  object CType extends net.chwthewke.scala.protobuf.ProtobufEnum[CType] {
    case object STRING extends CType("STRING", 0)
    case object CORD extends CType("CORD", 1)
    case object STRING_PIECE extends CType("STRING_PIECE", 2)

    val values: Vector[CType] = Vector(STRING, CORD, STRING_PIECE)

    def name(value: CType) = value.name
    def number(value: CType) = value.number
    implicit val protobufEnumInstance: net.chwthewke.scala.protobuf.ProtobufEnum[CType] = this
  }

}

