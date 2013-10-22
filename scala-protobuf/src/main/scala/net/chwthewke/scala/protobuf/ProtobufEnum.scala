package net.chwthewke.scala.protobuf

trait ProtobufEnum[V <: Numbered] {

  def values: IndexedSeq[V]

  trait Value {
    this: V =>
    def ordinal = values.indexOf(this)
  }

  def apply(ord: Int): V = values(ord)

  def named(name: String): Option[V] = values.find(_.name == name)
  def numbered(number: Int): Option[V] = values.find(_.number == number)

}
