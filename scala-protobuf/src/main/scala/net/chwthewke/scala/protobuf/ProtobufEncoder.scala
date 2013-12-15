package net.chwthewke.scala.protobuf

trait ProtobufEncoder[M] extends Encoder[M] {
  import FieldType._
  import WireFormat._

  implicit val M: Message[M]

  def run(m: M): Stream[Byte] = encoder.run(m)

  private def encoder: Encoder[M] = Encoder.sequence(M.fields map encodeField)

  private def encodeValue[A](fieldType: FieldType[A]): Encoder[A] = fieldType match {
    case Int64 | UInt64     => varIntEncoder
    case SInt64             => sint64Encoder
    case Int32 | UInt32     => int32Encoder
    case SInt32             => sint32Encoder
    case Bool               => boolEncoder
    case Fixed64 | SFixed64 => fixed64Encoder
    case Double             => doubleEncoder
    case Fixed32 | SFixed32 => fixed32Encoder
    case Float              => floatEncoder
    case Bytes              => bytesEncoder
    case String             => stringEncoder
    case Enum(pe)           => int32Encoder contramap pe.number
    case MessageField(m)    => ProtobufEncoder(m).encoder
  }

  private def encodeField(field: M.Field): Encoder[M] = field match {
    case _: M.Packed[_] => encodePacked(field)
    case _              => encodeNonPacked(field)
  }

  private def encodeNonPacked[T](field: M.Field): Encoder[M] =
    (encodeNonPackedHeader(field) ++ encodeValue(field.fieldType)).repeated.contramap(m => field.values(m))

  private def encodeNonPackedHeader(field: M.Field): Encoder[Any] =
    int32Encoder from fieldKey(field.number, field.fieldType.wireType)

  private def encodePacked[T](field: M.Field): Encoder[M] =
    encodePackedHeader(field) ++ (lengthPrefixed(encodeValue(field.fieldType).repeated)).contramap(m => field.values(m))

  private def encodePackedHeader(field: M.Field): Encoder[Any] =
    int32Encoder from fieldKey(field.number, WireType.LengthPrefixed)

  private def fieldKey(number: Int, wireType: WireType.t): Int = number << 3 | wireType

  private def lengthPrefixed[A](encoder: Encoder[A]): Encoder[A] = new Encoder[A] {
    override def run(in: A) = {
      val str = encoder.run(in)
      int32Encoder.run(str.length) #::: str
    }
  }

}

object ProtobufEncoder {
  def apply[M](implicit M0: Message[M]): ProtobufEncoder[M] = new ProtobufEncoder[M] { implicit val M: Message[M] = M0 }

}
