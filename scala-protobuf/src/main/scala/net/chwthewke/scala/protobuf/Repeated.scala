package net.chwthewke.scala.protobuf

trait Repeated[T, M] extends Field[Vector[T], Vector[T], M] {
  override def default: Vector[T] = Vector.empty
  override def eval(in: Vector[T]): Vector[T] = in
  override def lift(f: Vector[T]): Vector[T] = f

  override def merge(left: Vector[T], right: Vector[T]): Vector[T] = left ++ right

  def <=(ts: Iterable[T]): Update = set(ts.toVector)
  def <+=(t: T): Update = alter(_ :+ t)
  def <++=(ts: Iterable[T]): Update = alter(_ ++ ts)

}

object Repeated {
  def apply[T, M](name: String, number: Int, getter: M => Vector[T]) = {
    val (n, i) = (name, number)
    new Repeated[T, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): Vector[T] = getter(m)
    }
  }
}