package net.chwthewke.scala.protobuf

case class MessageDecl[+A](name: String, value: A, nestedMessages: List[MessageDecl[A]])

case class FileDecl[+A, +B](value: A, pkg: List[String], messages: List[MessageDecl[B]])
