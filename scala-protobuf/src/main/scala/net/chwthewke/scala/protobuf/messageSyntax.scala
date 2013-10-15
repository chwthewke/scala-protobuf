package net.chwthewke.scala.protobuf

import scala.language.implicitConversions

trait MessageOps[M] {
  implicit def M: Message[M]

  def self: M

  def builder = M.lift(self)

  def ++(other: M) = M.merge(self, other)

  def mod(mods: FieldUpdate[M]*): M = M.mod(self, mods)
}

trait ToMessageOps {
  implicit def toMessageOps[M](m: M)(implicit M0: Message[M]) =
    new MessageOps[M] { implicit def M = M0; def self = m }
}

object syntax extends ToMessageOps

trait MessageSyntax[M] {
  implicit def toMessageOps(m: M)(implicit M0: Message[M]) =
    new MessageOps[M] { implicit def M = M0; def self = m }
}
