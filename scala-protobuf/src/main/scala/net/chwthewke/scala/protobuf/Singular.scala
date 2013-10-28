package net.chwthewke.scala.protobuf

trait Singular[C, T, M] extends Field[C, T, M] {

  def <=(c: C): Update = set(Vector(c))
  def <|=(c: C): Update = alter(cs => (c +: cs).takeRight(1))
  override def <+=(c: C): Update = <=(c)

  override def merge(left: Vector[C], right: Vector[C]) = (left ++ right).takeRight(1)
}
