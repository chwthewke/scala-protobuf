package net.chwthewke.scala.protobuf

trait Field[I, O, M] {
  self =>

  trait Update {
    def field: Field[I, O, M] = self
    def apply(in: I): I
  }

  def name: String
  def number: Int

  def get(m: M): O
  def lift(f: O): I
  def default: I
  def eval(in: I): O
  def merge(left: I, right: I): I

  def unary_- : Update = set(default)

  protected def set(t: I): Update = new Update { override def apply(_t: I) = t }

  protected def alter(f: I => I): Update = new Update { override def apply(t: I) = f(t) }

}
