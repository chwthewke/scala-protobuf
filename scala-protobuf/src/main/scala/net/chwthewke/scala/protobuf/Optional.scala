package net.chwthewke.scala.protobuf

trait Optional[C, M] extends Singular[C, Option[C], M] {
  override def eval(in: Vector[C]): Option[C] = in.lastOption
  override def lift(f: Option[C]): Vector[C] = f.toVector

  // TODO def defaultForType : C ?
}

object Optional {
  def apply[C, M](name: String, number: Int, getter: M => Option[C]) = {
    val (n, i) = (name, number)
    new Optional[C, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): Option[C] = getter(m)
    }
  }
}
