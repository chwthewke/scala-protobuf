package net.chwthewke.scala.protobuf

trait Repeated[C, M] extends Field[C, Vector[C], M] {
  override def eval(in: Vector[C]): Vector[C] = in
  override def lift(f: Vector[C]): Vector[C] = f

  def <=(ts: Iterable[C]): Update = set(ts.toVector)
  def <+=(t: C): Update = alter(_ :+ t)
  def <++=(ts: Iterable[C]): Update = alter(_ ++ ts)

}

object Repeated {
  def apply[C, M](name: String, number: Int, getter: M => Vector[C]) = {
    val (n, i) = (name, number)
    new Repeated[C, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): Vector[C] = getter(m)
    }
  }
}
