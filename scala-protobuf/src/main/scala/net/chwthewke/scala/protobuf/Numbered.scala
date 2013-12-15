package net.chwthewke.scala.protobuf

trait Numbered[V] {
  def number(v: V): Int
  def name(v: V): String
}


object Numbered {
  implicit class NumberedOps[V](self: V)(implicit numbered: Numbered[V]) {
    def number: Int = numbered.number(self)
    def name: String = numbered.name(self)
  }
}
