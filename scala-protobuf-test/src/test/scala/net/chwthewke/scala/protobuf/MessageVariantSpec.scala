package net.chwthewke.scala.protobuf

import com.google.protobuf.{ByteString => PbByteString}
import org.scalatest.Matchers
import org.scalatest.WordSpec
import net.chwthewke.scala.protobuf.test.TestProtocol.AnEnum
import net.chwthewke.scala.protobuf.test.TestProtocol.LengthDelimited
import net.chwthewke.scala.protobuf.test.TestProtocol.PackedFields
import net.chwthewke.scala.protobuf.test.TestProtocol.Primitives
import net.chwthewke.scala.protobuf.test.TestProtocol.RepeatedFields
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ AnEnum => PbAnEnum }
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ LengthDelimited => PbLengthDelimited }
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ PackedFields => PbPackedFields }
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ Primitives => PbPrimitives }
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ RepeatedFields => PbRepeatedFields }

class MessageVariantSpec extends WordSpec with Matchers {

  "a MessageVariant for AnEnum" should {
    val enumVariant = MessageVariant.reflective[AnEnum, PbAnEnum](AnEnum)

    "convert enum value to its descriptor" in {
      enumVariant.enumVariant(AnEnum.Values, AnEnum.Values.ONE) should be(PbAnEnum.Values.ONE.getValueDescriptor)
    }

    "convert message w/ enum value" in {
      enumVariant.apply(AnEnum(Some(AnEnum.Values.ONE))) should equal(
        PbAnEnum.newBuilder().setValue(PbAnEnum.Values.ONE).build
      )
    }
  }

  "a MessageVariant for Primitives" should {

    val primitiveVariant = MessageVariant.reflective[Primitives, PbPrimitives](Primitives)

    "convert message with primitive fields" in {

      val message = Primitives(
        anInt32 = Some(4),
        anUInt32 = Some(3),
        aSInt32 = Some(-1),
        anInt64 = Some(4L),
        anUInt64 = Some(3L),
        aSInt64 = Some(-1L),
        aFixed32 = Some(1),
        aSFixed32 = Some(-3),
        aFixed64 = Some(1L),
        aSFixed64 = Some(-3L),
        aFloat = Some(0.01f),
        aDouble = Some(1.5d),
        aBool = Some(true))

      val expectedVariant = PbPrimitives.newBuilder
        .setAnInt32(4)
        .setAnUInt32(3)
        .setASInt32(-1)
        .setAnInt64(4L)
        .setAnUInt64(3L)
        .setASInt64(-1L)
        .setAFixed32(1)
        .setASFixed32(-3)
        .setAFixed64(1L)
        .setASFixed64(-3L)
        .setAFloat(0.01f)
        .setADouble(1.5d)
        .setABool(true)
        .build

      primitiveVariant.apply(message) should equal(expectedVariant)

    }
  }

  "a MessageVariant for LengthDelimited" should {
    val variant = MessageVariant.reflective[LengthDelimited, PbLengthDelimited](LengthDelimited)

    "convert a message with length-delimited fields" in {

      val message = LengthDelimited(aBytes = Some(ByteString(0x01, 0x02, 0x03, 0x7F)), aString = Some("abcd"))

      val expectedVariant = PbLengthDelimited.newBuilder
        .setABytes(PbByteString.copyFrom(Array[Byte](0x01, 0x02, 0x03, 0x7F)))
        .setAString("abcd")
      .build

      variant.apply(message) should equal(expectedVariant)

    }
  }

  "a MessageVariant for RepeatedFields" should {
    val variant = MessageVariant.reflective[RepeatedFields, PbRepeatedFields](RepeatedFields)

    "convert a message with repeated fields" in {
      val message = RepeatedFields(
        values = Vector(AnEnum.Values.ONE, AnEnum.Values.TWO),
        strings = Vector("abc", "def"),
        int32s = Vector(1, 2, 4))

      val expectedVariant = PbRepeatedFields.newBuilder
        .addValues(PbAnEnum.Values.ONE)
        .addValues(PbAnEnum.Values.TWO)
        .addStrings("abc")
        .addStrings("def")
        .addInt32S(1)
        .addInt32S(2)
        .addInt32S(4)
        .build

      variant.apply(message) should equal(expectedVariant)
    }
  }
  "a MessageVariant for PackedFields" should {
    val variant = MessageVariant.reflective[PackedFields, PbPackedFields](PackedFields)

    "convert a message with repeated fields" in {
      val message = PackedFields(
        values = Vector(AnEnum.Values.ONE, AnEnum.Values.TWO),
        int32s = Vector(1, 2, 4),
        int64s = Vector(1L, 3L))

      val expectedVariant = PbPackedFields.newBuilder
        .addValues(PbAnEnum.Values.ONE)
        .addValues(PbAnEnum.Values.TWO)
        .addInt32S(1)
        .addInt32S(2)
        .addInt32S(4)
        .addInt64S(1L)
        .addInt64S(3L)
        .build

      variant.apply(message) should equal(expectedVariant)
    }
  }

}
