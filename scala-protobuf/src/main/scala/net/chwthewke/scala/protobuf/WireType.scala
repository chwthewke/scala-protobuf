package net.chwthewke.scala.protobuf

object WireType {

  type t = Int

  val VarInt = 0
  val Fixed64 = 1
  val LengthPrefixed = 2
  val GroupStart = 3
  val GroupEnd = 4
  val Fixed32 = 5
}
