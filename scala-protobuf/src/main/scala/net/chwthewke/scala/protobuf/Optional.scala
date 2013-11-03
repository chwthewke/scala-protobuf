package net.chwthewke.scala.protobuf

sealed trait Optional[C, M] extends Singular[C, Option[C], M] {
  override def eval(in: Vector[C]): Option[C] = in.lastOption
  override def lift(f: Option[C]): Vector[C] = f.toVector

  // TODO def defaultForType : C ?
}

object Optional {
  def apply[C, M](name: String, number: Int, fieldType: FieldType[C], getter: M => Option[C]) = {
    val (n, i, f) = (name, number, fieldType)
    new Optional[C, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): Option[C] = getter(m)
      override def fieldType: FieldType[C] = f
    }
  }
}
