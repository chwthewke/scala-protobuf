package net.chwthewke.scala.protobuf

trait Required[T, M] extends Singular[T, T, M] {
  override def eval(in: Option[T]): T = in.getOrElse(throw new IllegalStateException(s"Unintialized field $name."))
  override def lift(f: T): Option[T] = Some(f)
}

object Required {
  def apply[T, M](name: String, number: Int, getter: M => T) = {
    val (n, i) = (name, number)
    new Required[T, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): T = getter(m)
    }
  }
}
