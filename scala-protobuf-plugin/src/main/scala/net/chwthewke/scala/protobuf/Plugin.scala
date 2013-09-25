package net.chwthewke.scala.protobuf

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse

trait Plugin {

  def process(request: CodeGeneratorRequest): CodeGeneratorResponse

}
