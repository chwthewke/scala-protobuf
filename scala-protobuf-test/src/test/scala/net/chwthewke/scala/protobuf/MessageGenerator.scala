package net.chwthewke.scala.protobuf

import com.google.protobuf.{ Message => PbMessage }
import com.google.protobuf.{ ByteString => PbByteString }
import com.google.protobuf.Message.{ Builder => PbBuilder }
import net.chwthewke.scala.protobuf.FieldType._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.reflect.ClassTag
import com.google.protobuf.ProtocolMessageEnum
import com.google.protobuf.Descriptors.EnumValueDescriptor

object MessageGenerator {
  import Arbitrary._

  implicit def arbitraryMessage[M](implicit M: Message[M]) = Arbitrary {
    new MessageGenerator.Of[M].generator
  }

  def messagePair[M: Message, R <: PbMessage]()(implicit variant: MessageVariant[M, R]): Gen[(M, R)] =
    for (m <- arbitrary[M]) yield m -> variant(m)

  class Of[M: Message](val m: Message[M]) {

    def this() = this(implicitly[Message[M]])

    def generator: Gen[M] = for {
      fs <- chooseFields[M]
      updates <- Gen.sequence[List, m.Field#Update](fs map (f => pickFieldValue(f)))
    } yield m.Builder(updates: _*).build

    def chooseFields[M]: Gen[List[m.Field]] = {
      val (required, notRequired) = m.fields partition {
        case f: m.Required[_] => true
        case _                => false
      }

      for {
        pickNotRequired <- Gen.someOf(notRequired)
      } yield (required ++ pickNotRequired).toList
    }

    def pickFieldValue(f: m.Field): Gen[f.Update] = {
      ValuesGen[f.type](f)(pickValue(f.fieldType)) map { xs => f.addAll(xs) }
    }

    case class ValuesGen[+T <: m.Field](val f: T) {
      def apply[X](g: Gen[X]): Gen[Vector[X]] = {
        implicit val arbX = Arbitrary(g)
        f match {
          case m.Required(_, _, _, _)                        => g map (x => Vector(x))
          case m.Optional(_, _, _, _)                        => arbitrary[Option[X]] map (x => x.toVector)
          case m.Repeated(_, _, _, _) | m.Packed(_, _, _, _) => arbitrary[List[X]] map (_.toVector)
        }
      }
    }

    def pickValue[T](f: FieldType[T]): Gen[T] = f match {
      case Int64 | UInt64 | SInt64 | Fixed64 | SFixed64 => arbitrary[Long]
      case Int32 | UInt32 | SInt32 | Fixed32 | SFixed32 => arbitrary[Int]
      case Bool                                         => arbitrary[Boolean]
      case Double                                       => arbitrary[Double]
      case Float                                        => arbitrary[Float]
      case Bytes                                        => arbitrary[Array[Byte]] map (ByteString.apply)
      case String                                       => arbitrary[String]
      case Enum(pe)                                     => Gen.oneOf(pe.values)
      case MessageField(m)                              => arbitraryMessage(m).arbitrary
    }

  }

}
