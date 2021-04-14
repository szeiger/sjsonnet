package sjsonnet

import java.io.{PrintWriter, StringWriter}

import fastparse.Parsed
import sjsonnet.Expr.Params

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Wraps all the machinery of evaluating Jsonnet source code, from parsing to
  * evaluation to materialization, into a convenient wrapper class.
  */
class Interpreter(extVars: Map[String, ujson.Value],
                  tlaVars: Map[String, ujson.Value],
                  wd: Path,
                  importer: (Path, String) => Option[(Path, String)],
                  preserveOrder: Boolean = false,
                  strict: Boolean = false,
                  storePos: Position => Unit = _ => (),
                  val parseCache: mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]] = new mutable.HashMap,
                  staticOpt: Boolean = true) {

  val resolver = new CachedResolver(parseCache) {
    override def process(expr: Expr, fs: FileScope): Either[String, (Expr, FileScope)] = {
      if(staticOpt)
        Right(((new StaticOptimizer(fs.nameIndices.size)(evaluator)).transform(expr), fs))
      else Right((expr, fs))
    }
  }

  val evaluator: Evaluator = new Evaluator(
    resolver,
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
      v <- evaluate(txt, path)
      r <- materialize(v, visitor)
    } yield r
  }

  def evaluate[T](txt: String, path: Path): Either[String, Val] = {
    for{
      res <- resolver.resolve(path, txt) match {
        case Left(msg) => Left("Parse error: " + msg)
        case r => r
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
          new Val.Func(f.pos, f.defSiteValScope, Params(f.params.names, defaults2, f.params.indices)) {
            def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position) = f.evalRhs(vs, es, fs, pos)
            override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope) = f.evalDefault(expr, vs, es)
          }
        case x => x
      }
    } yield res
  }

  def materialize[T](res: Val, visitor: upickle.core.Visitor[T, T]): Either[String, T] = {
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
  }
}
