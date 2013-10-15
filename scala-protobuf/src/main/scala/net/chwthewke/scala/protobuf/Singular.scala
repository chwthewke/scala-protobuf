package net.chwthewke.scala.protobuf

trait Singular[I, O, M] extends Field[Option[I], O, M] {
  override def default: Option[I] = None
  override def merge(left: Option[I], right: Option[I]): Option[I] = right.orElse(left)

  def <=(t: I): Update = set(Some(t))
  def <|=(t: I): Update = alter(_.orElse(Some(t)))
}
