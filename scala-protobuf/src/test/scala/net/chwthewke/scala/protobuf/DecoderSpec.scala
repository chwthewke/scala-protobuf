package net.chwthewke.scala.protobuf

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.GivenWhenThen

class DecoderSpec extends PropSpec with TableDrivenPropertyChecks with GivenWhenThen {

  val boolSeqs = Table("packed values",
    Vector(true, false, true),
    Vector(false, false),
    Vector(true))

  property("Packed field decoder decodes 'length field *'") {

    val decoder = new ProtobufDecoder[TestMessage] {
      implicit val M = TestMessage
    }.fieldDecoder(TestMessage.Fields.packedBool)

    forAll(boolSeqs) { (bs: Vector[Boolean]) =>
      val encoded = bs.length.toByte +: (bs map (b => (if (b) 0x01 else 0x00).toByte))

      Given(s"an encoded field $encoded")

      val updates: TestMessage.Fields.packedBool.Update = decoder.run(encoded)._2.get

      val decoded = TestMessage(requiredString = "") updated updates

      When("decoded with the packed field decoder")

      assert(decoded.packedBool === bs)

      Then("The decoded booleans are as the original")
    }
  }

  property("untilEmpty decodes all readable values from 'field *'") {
    val decoder = WireFormat.boolDecoder.untilEmpty

    forAll(boolSeqs) { (bs: Vector[Boolean]) =>
      val encoded = bs map (b => (if (b) 0x01 else 0x00).toByte)

      Given(s"an encoded sequence $encoded")

      val decoded = decoder.run(encoded)._2.get

      When("decoded with boolDecoder untilEmpty")

      assert(decoded == bs)

      Then("The decoded booleans are as the original")

    }
  }

}