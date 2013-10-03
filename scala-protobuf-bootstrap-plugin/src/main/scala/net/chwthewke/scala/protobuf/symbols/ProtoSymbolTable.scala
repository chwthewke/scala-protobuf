package net.chwthewke.scala.protobuf.symbols

case class ProtoSymbolTable(symbols: Vector[ProtoSymbol])

object ProtoSymbolTable {
  implicit class LookupOps(pst: ProtoSymbolTable) extends ProtoSymbolTableLookupOps {
    def self = pst
  }
}

