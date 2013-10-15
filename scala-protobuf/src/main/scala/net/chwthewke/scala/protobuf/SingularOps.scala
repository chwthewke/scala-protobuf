package net.chwthewke.scala.protobuf

trait SingularOps[I, O, M] {
  def self: Singular[I, O, M]

  def <=(p: Builder[I])(implicit M: Message[I]): Singular[I, O, M]#Update = self <= M.build(p)
  def <|=(p: Builder[I])(implicit M: Message[I]): Singular[I, O, M]#Update = self <|= M.build(p)
}