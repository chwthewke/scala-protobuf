package net.chwthewke.scala.protobuf

trait Field[C, T, M] extends Numbered {
  self =>

  trait Update {
    def field: Field[C, T, M] = self
    def apply(in: Vector[C]): Vector[C]
  }

  def get(m: M): T
  def lift(f: T): Vector[C]
  def eval(in: Vector[C]): T

  def merge(left: Vector[C], right: Vector[C]): Vector[C] = left ++ right

  def unary_- : Update = set(Vector.empty)

  protected def set(cs: Vector[C]): Update = new Update { override def apply(v: Vector[C]) = cs }

  protected def alter(f: Vector[C] => Vector[C]): Update = new Update { override def apply(cs: Vector[C]) = f(cs) }

}
