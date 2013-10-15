package net.chwthewke.scala.protobuf

import scala.language.implicitConversions

class Builder[M] private[protobuf] (private[Builder] val fields: Map[Field[_, _, M], Any]) {

  def get[I](field: Field[I, _, M]): I = fields.get(field).fold(field.default)(_.asInstanceOf[I])

  def eval[I, O](field: Field[I, O, M]): O = field.eval(get(field))

  def update[I, O](mod: Field[I, O, M]#Update): Builder[M] =
    new Builder(fields + (mod.field -> mod.apply(get(mod.field))))

  def apply(mods: FieldUpdate[M]*) = (this /: mods) { _.update(_) }

  def ++(other: Builder[M]) = new Builder(fields ++ other.fields)

  def build(implicit M: Message[M]) = M.build(this)
}

object Builder {

  def apply[M](mods: FieldUpdate[M]*)(implicit M: Message[M]): Builder[M] =
    new Builder[M](Map()).apply(mods: _*)

  implicit def toSingularOps[I, O, M](field: Singular[I, O, M]) =
    new SingularOps[I, O, M] { def self = field }

  implicit def toRepeatedOps[T, M](field: Repeated[T, M]) =
    new RepeatedOps[T, M] { def self = field }
}
