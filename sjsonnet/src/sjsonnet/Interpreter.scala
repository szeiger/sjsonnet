package sjsonnet

import java.io.{PrintWriter, StringWriter}

import fastparse.Parsed
import sjsonnet.Expr.Params
import Namer.Name

import scala.util.control.NonFatal

/**
  * Wraps all the machinery of evaluating Jsonnet source code, from parsing to
  * evaluation to materialization, into a convenient wrapper class.
  */
class Interpreter(parseCache: collection.mutable.HashMap[String, fastparse.Parsed[(Expr, FileScope)]],
                  namer: Namer,
                  extVars: Map[Name, ujson.Value],
                  tlaVars: Map[Name, ujson.Value],
                  wd: Path,
                  importer: (Path, String) => Option[(Path, String)],
                  preserveOrder: Boolean = false,
                  strict: Boolean = false,
                  storePos: Position => Unit = _ => ()) {

  val evaluator = new Evaluator(
    parseCache,
    namer,
    extVars,
    wd,
    importer,
    preserveOrder,
    strict
  )

  def interpret(txt: String, path: Path): Either[String, ujson.Value] = {
    interpret0(txt, path, ujson.Value)
  }
  def interpret0[T](txt: String,
                    path: Path,
                    visitor: upickle.core.Visitor[T, T]): Either[String, T] = {
    for{
      res <- parseCache.getOrElseUpdate(txt, fastparse.parse(txt, new Parser(path, namer).document(_))) match{
        case f @ Parsed.Failure(l, i, e) => Left("Parse error: " + f.trace().msg)
        case Parsed.Success(r, index) => Right(r)
      }
      (parsed, newFileScope) = res
      _ = evaluator.loadedFileContents(path) = txt
      res0 <-
        try Right(
          evaluator.visitExpr(parsed)(
            Std.scope(newFileScope.nameIndices.size + 1)
          )
        )
        catch{case NonFatal(e) =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
        }
      res = res0 match{
        case f: Val.Func =>
          val defaults2 = f.params.defaultExprs.clone()
          var i = 0
          while(i < defaults2.length) {
            tlaVars.get(f.params.names(i)) match {
              case Some(v) => defaults2(i) = Materializer.toExpr(v)(evaluator)
              case None =>
            }
            i += 1
          }
          f.copy(params = Params(f.params.names, defaults2, f.params.indices))
        case x => x
      }
      json <-
        try Right(Materializer.apply0(res, visitor, storePos = storePos)(evaluator))
        catch{
          case Error.Delegate(msg) => Left(msg)
          case NonFatal(e) =>
            val s = new StringWriter()
            val p = new PrintWriter(s)
            e.printStackTrace(p)
            p.close()
            Left(s.toString.replace("\t", "    "))
        }
    } yield json
  }
}
