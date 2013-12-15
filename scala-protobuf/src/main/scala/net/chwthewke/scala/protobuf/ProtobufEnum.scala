package net.chwthewke.scala.protobuf

trait ProtobufEnum[V] extends Numbered[V] {

  def values: IndexedSeq[V]

  trait Value {
    this: V =>
    def ordinal = values.indexOf(this)
  }

  def apply(ord: Int): V = values(ord)

  def named(name: String): Option[V] = values.find(this.name(_) == name)
  def numbered(number: Int): Option[V] = values.find(this.number(_) == number)

}

