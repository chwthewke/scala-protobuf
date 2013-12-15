package net.chwthewke.scala.protobuf

import com.google.protobuf.{ Message => PbMessage, ByteString => PbByteString, ProtocolMessageEnum }
import com.google.protobuf.Descriptors.EnumValueDescriptor
import com.google.protobuf.Message.{ Builder => PbBuilder }
import net.chwthewke.scala.protobuf.FieldType._
import scala.reflect.ClassTag

trait MessageVariant[M, +R <: PbMessage] extends (M => R) {

  implicit val M: Message[M]

  type B <: PbBuilder

  def buildPb(b: B): R
  def newPbBuilder: B

  def enumVariant[T](pe: ProtobufEnum[T], x: T): Any
  def messageVariant[T](m: Message[T], x: T): Any

  override def apply(m: M): R = {

    val fieldBuilders: Seq[B => B] = M.fields map (builder(m, _) andThen (_.asInstanceOf[B]))

    buildPb((fieldBuilders :\ newPbBuilder)(_(_)))
  }

  def builder(m: M, f: M.Field): PbBuilder => PbBuilder = f match {
    case M.Required(_, number, ft, g) => b => concSingle(number)(b, variant(ft, g(m)))
    case M.Optional(_, number, ft, g) => b => iterateBuilder(b, g(m) map (variant(ft, _)), concSingle(number))
    case M.Repeated(_, number, ft, g) => repeatedBuilder(m, number, ft, g)
    case M.Packed(_, number, ft, g) => repeatedBuilder(m, number, ft, g)
  }

  def repeatedBuilder[T](m: M, number: Int, ft: FieldType[T], g: M => Vector[T]): PbBuilder => PbBuilder =
    b => iterateBuilder(b, g(m) map (variant(ft, _)), concRepeated(number))

  def iterateBuilder(b: PbBuilder, items: Iterable[Any], conc: (PbBuilder, Any) => PbBuilder) =
    (b /: items)(conc)

  def concSingle(fieldNumber: Int)(b: PbBuilder, x: Any): PbBuilder = {
    b.setField(pbField(b, fieldNumber), x)
  }
  def concRepeated(fieldNumber: Int)(b: PbBuilder, x: Any): PbBuilder = {
    b.addRepeatedField(pbField(b, fieldNumber), x)
  }

  def variant[T](ft: FieldType[T], x: T): Any = ft match {
    case Enum(pe)         => enumVariant(pe, x)
    case MessageField(m1) => messageVariant(m1, x)
    case Bytes            => toPbByteString(x)
    case Int32 | SInt32 |
         UInt32 | Fixed32 |
         SFixed32         => int2Integer(x)
    case Int64 | SInt64 |
         UInt64 | Fixed64 |
         SFixed64         => long2Long(x)
    case Bool             => boolean2Boolean(x)
    case Float            => float2Float(x)
    case Double           => double2Double(x)
    case o                => x
  }

  def toPbByteString(bs: ByteString): PbByteString = PbByteString.copyFrom(bs.toArray)

  def pbField(b: PbBuilder, number: Int) = b.getDescriptorForType().findFieldByNumber(number)

}

object MessageVariant {

  private[protobuf] class ReflectionVariant[M, R <: PbMessage](override val M: Message[M])(implicit CT: ClassTag[M]) extends MessageVariant[M, R] {
    type B = PbBuilder

    override def buildPb(b: B): R = b.build.asInstanceOf[R]
    override def newPbBuilder: B =
      classToRef(CT.runtimeClass).getDeclaredMethod("newBuilder").invoke(null).asInstanceOf[PbBuilder]

    private def classToRef(c: Class[_]): Class[_] = {
      val name = c.getName
      val pbName = {
        val parts = name.split('.'): IndexedSeq[String]
        (parts.init ++ Seq("ref", parts.last)) mkString "."
      }
      Class.forName(pbName)
    }

    override def enumVariant[T](pe: ProtobufEnum[T], x: T): EnumValueDescriptor = {
      val pbEnumClass = classToRef(x.getClass.getSuperclass)
      pbEnumClass.getField(pe.name(x)).get(null).asInstanceOf[ProtocolMessageEnum].getValueDescriptor
    }
    override def messageVariant[T](m: Message[T], x: T) = new ReflectionVariant(m)(ClassTag(x.getClass)) apply x
  }

  def reflective[M, R <: PbMessage](m: Message[M])(implicit CT: ClassTag[M]): MessageVariant[M, R] =
    new ReflectionVariant[M, R](m)

}
