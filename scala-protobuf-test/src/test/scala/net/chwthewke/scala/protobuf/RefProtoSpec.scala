package net.chwthewke.scala.protobuf

import scala.reflect.ClassTag

import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import com.google.{ protobuf => pb }

import net.chwthewke.scala.protobuf.scalautils.Good
import net.chwthewke.scala.protobuf.scalautils.Or
import net.chwthewke.scala.protobuf.test.TestProtocol._
import net.chwthewke.scala.protobuf.test.ref

class RefProtoSpec extends WordSpec with GeneratorDrivenPropertyChecks with Matchers with GivenWhenThen {

  "Primitives messages" should {
    behave like new RefProto[Primitives, ref.TestProtocol.Primitives](ref.TestProtocol.Primitives.PARSER).behaviour
  }

  "AnEnum messages" should {
    behave like new RefProto[AnEnum, ref.TestProtocol.AnEnum](ref.TestProtocol.AnEnum.PARSER).behaviour
  }

  "LargeFieldNumbers messages" should {
    behave like new RefProto[LargeFieldNumbers, ref.TestProtocol.LargeFieldNumbers](ref.TestProtocol.LargeFieldNumbers.PARSER).behaviour
  }

  "LengthDelimited messages" should {
    behave like new RefProto[LengthDelimited, ref.TestProtocol.LengthDelimited](ref.TestProtocol.LengthDelimited.PARSER).behaviour
  }

  "PackedFields messages" should {
    behave like new RefProto[PackedFields, ref.TestProtocol.PackedFields](ref.TestProtocol.PackedFields.PARSER).behaviour
  }

  "RepeatedFields messages" should {
    behave like new RefProto[RepeatedFields, ref.TestProtocol.RepeatedFields](ref.TestProtocol.RepeatedFields.PARSER).behaviour
  }

  class RefProto[M, R <: pb.Message](val parser: pb.Parser[R])(implicit ct: ClassTag[M], m: Message[M]) {

    private implicit def variant = MessageVariant.reflective(m)

    def pairGen: Gen[(M, R)] = MessageGenerator.messagePair[M, R]

    private def S2R(msg: M): R = parser.parseFrom(ProtobufEncoder[M].run(msg).toArray)
    private def R2S(refMsg: R): M Or Decoder.Error = Parser[M].parseFrom(refMsg.toByteArray)

    private def truncMsg(x: Any) = x.toString.splitAt(40) match {
      case (s, "") => s
      case (s, _)  => s + "..."
    }

    def scalaToRef = {
      forAll(pairGen) {
        case ((msg, r)) =>
          Given(s"a message ${truncMsg(msg)}")

          When("serialized and decoded with google protobuf")

          val conv = S2R(msg)

          Then(s"results in ${truncMsg(conv)}")

          conv should equal(r)
      }
    }

    def refToScala = {
      forAll(pairGen) {
        case ((msg, r)) =>
          Given(s"a google protobuf message ${truncMsg(r)}")

          When("serialized and decoded with scala protobuf")

          val conv = R2S(r)

          Then(s"results in ${truncMsg(conv)}")

          conv should equal(Good(msg))
      }
    }

    def scalaToScala = {
      forAll(pairGen) {
        case ((msg, r)) =>
          Given(s"a message ${truncMsg(msg)}")

          When("converted to google protobuf and back")

          val conv = R2S(S2R(msg))

          Then(s"results in ${truncMsg(conv)}")

          conv should equal(Good(msg))
      }
    }

    def refToRef = {
      forAll(pairGen) {
        case ((msg, r)) =>
          Given(s"a google protobuf message ${truncMsg(r)}")

          When("converted to scala protobuf and back")

          val conv = R2S(r) map S2R

          Then(s"results in ${truncMsg(conv)}")

          conv should equal(Good(r))
      }
    }

    def behaviour = {
      "translate from pb to scala" in refToScala
      "translate from scala to pb" in scalaToRef

      "translate from scala to pb to scala" in scalaToScala
      "translate from pb to scala to pb" in refToRef

    }
  }
}