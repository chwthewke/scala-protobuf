package net.chwthewke.scala

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import scala.language.implicitConversions
import scalaz.{ReaderWriterState, RWST, Id}
import scalaz.std.vector._
import scalaz.syntax.{MonadListenOps, MonadTellOps}

package object protobuf {

  type Process[X] = ReaderWriterState[CodeGeneratorRequest, Vector[String], Unit, X]

  type ProcessW[W, X] = ReaderWriterState[CodeGeneratorRequest, W, Unit, X]

  private implicit val rwstM =
    RWST.rwstMonad[Id.Id, CodeGeneratorRequest, Vector[String], Unit]

  object Process {
    def apply[X](x: X): Process[X] = rwstM.point(x)

    def apply[X](f: CodeGeneratorRequest => (Vector[String], X)): Process[X] = ReaderWriterState {
      (r, _) => {
        f(r) match {
          case (w, x) => (w, x, ())
        }
      }
    }

    def ask: Process[CodeGeneratorRequest] = rwstM.ask
  }


  implicit def ToProcessTellOps[X](p: Process[X]): MonadTellOps[ProcessW, Vector[String], X] = {
    scalaz.syntax.monadTell.ToMonadTellOps[ProcessW, X, Vector[String]](p)
  }

  implicit def ToProcessListenOps[X](p: Process[X]): MonadListenOps[ProcessW, Vector[String], X] = {
    scalaz.syntax.monadListen.ToMonadListenOps[ProcessW, X, Vector[String]](p)
  }



}
