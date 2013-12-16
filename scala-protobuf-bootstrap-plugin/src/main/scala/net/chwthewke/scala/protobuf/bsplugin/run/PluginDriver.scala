package net.chwthewke.scala.protobuf.bsplugin.run

import net.chwthewke.scala.protobuf.bsplugin._
import scala.util.Try
import scala.util.control.NonFatal
import java.io.PrintWriter
import java.io.StringWriter

trait PluginDriver {

  import interface._

  def log(s: String): Unit = {}

  def io: IO

  private def run(r: CodeGeneratorRequest)(p: Process[CodeGeneratorResponse]): CodeGeneratorResponse =
    p.run(r, ()) match { case (s, resp, _) => s.foreach(log); resp }

  private def respond(p: Process[CodeGeneratorResponse]): Try[CodeGeneratorResponse] =
    for {
      request <- io.parseFrom(System.in)
      response <- Try(run(request)(p))
    } yield response

  private def safe(response: Try[CodeGeneratorResponse]): CodeGeneratorResponse =
    response.recover {
      case NonFatal(e) =>
        CodeGeneratorResponse(Nil, Some(e.toString + "\n" + stackTraceOf(e)))
    }.get

  private def stackTraceOf(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    val res = Try { e.printStackTrace(pw); sw.toString }
    pw.close
    res.get
  }

  def run(p: Process[CodeGeneratorResponse]): Unit = io.write(safe(respond(p)))

}
