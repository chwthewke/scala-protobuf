package net.chwthewke.scala.protobuf

sealed trait Required[C, M] extends Singular[C, C, M] {
  override def eval(in: Vector[C]): C = in.lastOption.getOrElse(throw new IllegalStateException(s"Unintialized field $name."))
  override def lift(f: C): Vector[C] = Vector(f)
}

object Required {
  def apply[C, M](name: String, number: Int, fieldType: FieldType[C], getter: M => C): Required[C, M] = {
    val (n, i, f) = (name, number, fieldType)
    new Required[C, M] {
      override def name: String = n
      override def number: Int = i
      override def get(m: M): C = getter(m)
      override def fieldType: FieldType[C] = f
    }
  }
}
