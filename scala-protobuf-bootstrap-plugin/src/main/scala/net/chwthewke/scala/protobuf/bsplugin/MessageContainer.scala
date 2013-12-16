package net.chwthewke.scala.protobuf.bsplugin

import interface._

trait MessageContainer[A] {
  def messages(a: A): Vector[Descriptor]

  def enums(a: A): Vector[EnumDescriptor]

  def messageType: Int

  def enumType: Int
}

object MessageContainer {

  def apply[A](implicit MC: MessageContainer[A]): MessageContainer[A] = MC

}

