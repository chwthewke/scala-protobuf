package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorResponse, CodeGeneratorRequest}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.util.Try
import scala.util.control.NonFatal

object PluginMain extends PluginRunner {

  val plugin : Plugin = new Plugin {}


  def main(arg: Array[String]): Unit = run.get

}

trait PluginRunner {

  def plugin : Plugin

  def input = CodedInputStream.newInstance(System.in)
  def output = CodedOutputStream.newInstance(System.out)


  def run = Try(process()).recover {
    case NonFatal(e) => CodeGeneratorResponse.newBuilder
      .setError(e.toString)
      .build
  }

  def process() : Unit =
    plugin.process(CodeGeneratorRequest.parseFrom(input)).writeTo(output)

}
