package net.chwthewke.scala.protobuf

trait Optional[T, M] extends Singular[T, Option[T], M] {
  override def eval(in: Option[T]): Option[T] = in
  override def lift(f: Option[T]): Option[T] = f

  // TODO def defaultForType : T ?
}

object Optional {
  def apply[T, M](name: String, number: Int, getter: M => Option[T]) = {
    val (n, i) = (name, number)
    new Optional[T, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): Option[T] = getter(m)
    }
  }
}
