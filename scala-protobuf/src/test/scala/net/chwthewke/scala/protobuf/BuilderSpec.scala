package net.chwthewke.scala.protobuf

import org.scalatest.WordSpec
import org.scalatest.Matchers
import TestMessage.Fields._

class BuilderSpec extends WordSpec with Matchers {

  "A Builder of TestMessage" when {
    "All fields are set" should {
      val builder = TestMessage.Builder(
        optionalInt <= 3,
        requiredString <= "foo",
        repeatedBool <++= Seq(true, false, true),
        packedBool <++= Seq(false, true)
      )

      "build to the expected TestMessage" in {

        builder.build should be(TestMessage(Some(3), "foo", Vector(true, false, true), Vector(false, true)))

      }

    }
  }

}
