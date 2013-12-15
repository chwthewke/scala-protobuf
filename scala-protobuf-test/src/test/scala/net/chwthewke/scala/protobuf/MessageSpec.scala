package net.chwthewke.scala.protobuf

import org.scalatest.{Matchers, FlatSpec}
import net.chwthewke.scala.protobuf.test.TestProtocol.Primitives

class MessageSpec extends FlatSpec with Matchers {
  "Primitives with aBool = true field update" should "have aBool = Some(true)" in {
    val message = Primitives() updated (Primitives.Fields.aBool <= true)

    message should equal(Primitives(aBool = Some(true)))
  }

  "Primitives builder with aBool = true field update" should "have aBool = Some(true)" in {
    val message = Primitives.newBuilder mod (Primitives.Fields.aBool <= true)

    message.build should equal(Primitives(aBool = Some(true)))
  }
}
