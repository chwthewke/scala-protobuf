package net.chwthewke.scala.protobuf

import scala.language.implicitConversions

class Builder[M] private[protobuf] (fieldSet: Map[Field[_, _, M], Vector[Any]]) {

  val fields = fieldSet.withDefault(_ => Vector.empty)

  def get[C](field: Field[C, _, M]): Vector[C] = fields(field).asInstanceOf[Vector[C]]

  def eval[C, T](field: Field[C, T, M]): T = field.eval(get(field))

  def update[C, T](mod: Field[C, T, M]#Update): Builder[M] =
    new Builder(fields + (mod.field -> mod.apply(get(mod.field))))

  def apply(mods: FieldUpdate[M]*) = (this /: mods) { _ update _ }

  def ++(other: Builder[M]) = {
    val kvs = for {
      k <- (fields.keySet union other.fields.keySet).toSeq.sortBy(_.number)
    } yield (k, mergeField(k, other))
    new Builder[M](kvs.toMap)
  }

  private def mergeField[C, T](field: Field[C, T, M], other: Builder[M]): Vector[C] =
    field.merge(get(field), other.get(field))

  def build(implicit M: Message[M]) = M.build(this)
}

object Builder {

  def apply[M](mods: FieldUpdate[M]*)(implicit M: Message[M]): Builder[M] =
    new Builder[M](Map()).apply(mods: _*)

  implicit def toSingularOps[C, T, M](field: Singular[C, T, M]) =
    new SingularOps[C, T, M] { def self = field }

  implicit def toRepeatedOps[T, M](field: Repeated[T, M]) =
    new RepeatedOps[T, M] { def self = field }
}
