package net.chwthewke.scala.protobuf

trait RepeatedOps[T, M] {
  def self: Repeated[T, M]

  def <=(ms: Iterable[Builder[T]])(implicit M: Message[T]): Repeated[T, M]#Update = self <= ms.map(M.build)
  def <+=(m: Builder[T])(implicit M: Message[T]): Repeated[T, M]#Update = self <+= M.build(m)
  def <++=(ms: Iterable[Builder[T]])(implicit M: Message[T]): Repeated[T, M]#Update = self <++= ms.map(M.build)
}