package net.chwthewke.scala.protobuf

import net.chwthewke.scala.protobuf.scalautils.Good
import net.chwthewke.scala.protobuf.scalautils.Or
import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class WireFormatSpec extends PropSpec with TableDrivenPropertyChecks {

  val int32s = Table[IndexedSeq[Int], Int Or Decoder.Error](
    "encoded" -> "decoded",
    Vector(0x03) -> Good(3),
    Vector(0x81, 0x01) -> Good(129),
    Vector(0xF4, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01) -> Good(-12))

  property("int32s are decoded as expected") {

    val decoder = WireFormat.int32Decoder

    forAll(int32s) { (in: IndexedSeq[Int], out: Int Or Decoder.Error) =>

      assert(decoder.run(in map (_.toByte))._2 === out)

      info.apply(s"${in map (_.toHexString) mkString " "} decodes to $out")
    }

  }

  val fixed32s = Table(
    "encoded" -> "decoded",
    Vector(0x03, 0x00, 0x00, 0x00) -> Good(3),
    Vector(0x01, 0xA0, 0x01, 0x07) -> Good(0x0701A001)
  )

  property("fixed32s are decoded as expected") {

    val decoder = WireFormat.fixed32Decoder

    forAll(fixed32s) { (in: IndexedSeq[Int], out: Int Or Decoder.Error) =>

      assert(decoder.run(in map (_.toByte))._2 === out)

      info.apply(s"${in map (_.toHexString) mkString " "} decodes to $out")
    }

  }

}

object BytesDecoder {
  def apply(n: Int): Decoder[IndexedSeq[Byte]] = new WireFormat.BytesDecoder(n)
}
