package net.chwthewke.scala.protobuf

trait Message[M] {

  implicit def M: Message[M] = this

  def fields: Vector[Field[_, _, M]]

  def build(p: Builder[M]): M

  def lift(m: M): Builder[M] = {
    def liftField[C, T](field: Field[C, T, M]): Vector[C] =
      field.lift(field.get(m))

    new Builder[M](fields.map(f => f -> liftField(f)).toMap)
  }

  def merge(left: M, right: M): M = build(lift(left) ++ lift(right))

  def mod(m: M, mods: Seq[FieldUpdate[M]]): M =
    build(lift(m) ++ Builder[M](mods: _*))
}

object Message {
  def apply[M: Message] = implicitly[Message[M]]
}