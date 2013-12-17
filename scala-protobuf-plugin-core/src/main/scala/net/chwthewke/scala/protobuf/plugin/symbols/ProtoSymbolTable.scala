package net.chwthewke.scala.protobuf.plugin.symbols

case class ProtoSymbolTable(symbols: Vector[ProtoSymbol])

object ProtoSymbolTable {
  implicit class LookupOps(pst: ProtoSymbolTable) extends ProtoSymbolTableLookupOps {
    def self = pst
  }
}

