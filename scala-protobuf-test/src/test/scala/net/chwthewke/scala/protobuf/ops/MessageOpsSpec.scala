package net.chwthewke.scala.protobuf.ops

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import net.chwthewke.scala.protobuf.test.TestProtocol.Primitives

class MessageOpsSpec extends FlatSpec with Matchers {
  import net.chwthewke.scala.protobuf.ops.message._

  "A Message instance" should "be serializable with an op" in {

    val message = Primitives()

    message.toByteStream should equal(Stream.empty[Byte])

  }

}