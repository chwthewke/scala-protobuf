package net.chwthewke.scala.protobuf

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import net.chwthewke.scala.protobuf.test.TestProtocol.Primitives
import net.chwthewke.scala.protobuf.test.TestProtocol.PackedFields
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ Primitives => RefPrimitives }
import net.chwthewke.scala.protobuf.test.ref.TestProtocol.{ PackedFields => RefPackedFields }
import net.chwthewke.scala.protobuf.scalautils.Good

class RefDecoderSpec extends FlatSpec with Matchers {

  val `true` = Vector[Byte](0x01)
  val aBoolTrue = Vector[Byte](0x68, 0x01)

  val primitivesDecoders = new ProtobufDecoder[Primitives] { val M = Primitives }

  "the field decoder of Primitives.aBool" should "decode (1) into aBool = true" in {
    val fieldDec = primitivesDecoders.fieldDecoder(Primitives.Fields.aBool)

    val result = fieldDec.run(`true`)

    result._1 shouldBe 'empty

    val update = result._2
    update shouldBe 'Good
    Primitives() updated update.get should equal(Primitives(aBool = Some(true)))
  }

  "the 'any field' decoder of Primitives" should "decode (104, 1) into aBool = true" in {
    val anyFieldDec = primitivesDecoders.anyFieldDecoder

    val result = anyFieldDec.run(aBoolTrue)

    result._1 shouldBe 'empty

    val update = result._2
    update shouldBe 'Good
    Primitives() updated update.get should equal(Primitives(aBool = Some(true)))
  }

  "the decoder of Primitives" should "decode (104, 1) into aBool = true" in {
    val messageDec = primitivesDecoders.decoder

    val result = messageDec.run(aBoolTrue)

    result._1 shouldBe 'empty

    val message = result._2
    message shouldBe 'Good
    message.get should equal(Primitives(aBool = Some(true)))
  }

  "the encoding of RefPrimitives(aBool = true)" should "be (104, 1)" in {
    val message = RefPrimitives.newBuilder.setABool(true).build

    val bytes = message.toByteArray(): IndexedSeq[Byte]

    bytes should equal(Vector[Byte](0x68, 0x01))
  }

  "the parser of Primitives" should "decode (104, 1) into aBool = true" in {
    val parser = Parser[Primitives]

    val message = parser.parseFrom(aBoolTrue.toArray)

    message shouldBe 'Good
    message.get should equal(Primitives(aBool = Some(true)))
    message.get should equal(Primitives() updated (Primitives.Fields.aBool <= true))
  }

  val packedFieldsDecoder = new ProtobufDecoder[PackedFields] { val M = PackedFields }

  "the encoding of RefPackedFields with int32s = [ 257, 10 ]" should "be (10, 3, 129, 2, 10)" in {

    val msg = RefPackedFields.newBuilder.addInt32S(257).addInt32S(10).build

    val bytes = msg.toByteArray: IndexedSeq[Byte]

    bytes should equal(Vector[Byte](0x0A, 0x03, -0x7F, 0x02, 0x0A))
  }

  "the field decoder for int32s" should "decode (3, 129, 2, 10) to int32s = Vector(257, 10)" in {
    val decoder = packedFieldsDecoder.fieldDecoder(PackedFields.Fields.int32s)

    val bytes = Vector[Byte](0x03, -0x7F, 0x02, 0x0A)

    val (rem, update) = decoder.run(bytes)
    rem.isEmpty should be(true)
    update shouldBe 'good
    PackedFields() updated update.get should equal(PackedFields(int32s = Vector(257, 10)))
  }

  "the any field decoder for PackedFields" should "decode (10, 3, 129, 2, 10) to int32s = Vector(257, 10)" in {
    val decoder = packedFieldsDecoder.anyFieldDecoder

    val bytes = Vector[Byte](0x0A, 0x03, -0x7F, 0x02, 0x0A)

    val (rem, update) = decoder.run(bytes)
    rem.isEmpty should be(true)
    update shouldBe 'good
    PackedFields() updated update.get should equal(PackedFields(int32s = Vector(257, 10)))
  }
}
