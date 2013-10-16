package net.chwthewke.scala.protobuf.bsplugin.run

import com.google.protobuf.compiler.PluginProtos.{ CodeGeneratorRequest, CodeGeneratorResponse }
import net.chwthewke.scala.protobuf.bsplugin._
import scala.util.Try
import scala.util.control.NonFatal
import java.io.PrintWriter
import java.io.StringWriter

trait PluginDriver {

  def log(s: String): Unit = {}

  private def run(r: CodeGeneratorRequest)(p: Process[CodeGeneratorResponse]): CodeGeneratorResponse =
    p.run(r, ()) match { case (s, resp, _) => s.foreach(log); resp }

  private def respond(p: Process[CodeGeneratorResponse]): Try[CodeGeneratorResponse] =
    for {
      request <- Try(CodeGeneratorRequest.parseFrom(System.in))
      response <- Try(run(request)(p))
    } yield response

  private def safe(response: Try[CodeGeneratorResponse]): CodeGeneratorResponse =
    response.recover {
      case NonFatal(e) =>
        CodeGeneratorResponse.newBuilder
          .setError(e.toString + "\n" + stackTraceOf(e))
          .build
    }.get

  private def stackTraceOf(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    val res = Try { e.printStackTrace(pw); sw.toString }
    pw.close
    res.get
  }

  def run(p: Process[CodeGeneratorResponse]): Unit = safe(respond(p)).writeTo(System.out)

}
