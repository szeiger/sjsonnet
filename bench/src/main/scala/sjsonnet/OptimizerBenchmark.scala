package sjsonnet

import java.io.StringWriter
import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class OptimizerBenchmark {

  private var interp: Interpreter = _
  private var inputs: Iterable[(Expr, FileScope)] = _

  @Setup
  def setup(): Unit = {
    val parser = mainargs.ParserForClass[Config]
    val config = parser.constructEither(MainBenchmark.mainArgs, autoPrintHelpAndExit = None).getOrElse(???)
    val file = config.file
    val wd = os.pwd
    val path = OsPath(os.Path(file, wd))
    this.interp = new Interpreter(
      Map.empty[String, ujson.Value],
      Map.empty[String, ujson.Value],
      OsPath(wd),
      importer = SjsonnetMain.resolveImport(config.jpaths.map(os.Path(_, wd)).map(OsPath(_)), None),
      staticOpt = false,
      globalOpt = false
    )
    val writer = new StringWriter
    val renderer = new Renderer(writer, indent = 3)
    interp.interpret0(interp.resolver.read(path).get, path, renderer).getOrElse(???)
    inputs = interp.parseCache.values.map(_.getOrElse(???)).toIndexedSeq
    val static = inputs.map {
      case (expr, fs) => ((new StaticOptimizer(fs)(interp.evaluator)).transform(expr), fs)
    }
    val main = interp.parseCache.find { case ((p, _), _) => p == path }.get._2.getOrElse(???)
    val interpPaths = interp.parseCache.keySet.map(_._1)
    val mainOpt = new GlobalOptimizer(interp.resolver)
    mainOpt.optimize(main._1)
    val countBefore, countStatic, countGlobal = new Counter
    inputs.foreach(t => assert(countBefore.transform(t._1) eq t._1))
    static.foreach(t => assert(countStatic.transform(t._1) eq t._1))
    mainOpt.allDocs.values.foreach { d => countGlobal.transform(d.expr) }
    System.err.println(s"Documents: total=${inputs.size}, globalOpt=${mainOpt.allDocs.size}")

    //val diff = mainOpt.allDocs.keySet.diff(interpPaths)
    //System.err.println("Diff: "+diff)

    val strict = inputs.map { case (expr, fs) =>
      val sa = new StrictnessAnalyzer(fs, expr eq main._1)
      sa.transform(expr)
      sa.strictImports.foreach { e =>
        System.err.println(s"  strict import: $e")
      }
      sa.strict.size()
    }

    System.err.println(s"Strict Exprs: ${strict.sum}")

    val sa = new StrictnessAnalyzer(main._2, true)
    sa.transform(main._1)
    System.err.println(s"")

    System.err.println(s"Before: $countBefore")
    System.err.println(s"Static: $countStatic")
    System.err.println(s"Global: $countGlobal")
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (expr, fs) =>
      bh.consume((new StaticOptimizer(fs)(interp.evaluator)).transform(expr))
    })
  }

  class Counter extends ExprTransform {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
      otherObjs = 0
    def transform(e: Expr) = {
      total += 1
      if(e.isInstanceOf[Val]) vals += 1
      else exprs += 1
      e match {
        case _: Val.Arr => arrVals += 1
        case a: Expr.Arr =>
          if(a.value.forall(_.isInstanceOf[Val])) staticArrExprs += 1
          else otherArrExprs += 1
        case _: Val.Obj => staticObjs += 1
        case e: Expr.ObjBody.MemberList =>
          if(e.binds == null && e.asserts == null && e.fields.forall(_.isStatic)) missedStaticObjs += 1
          else otherObjs += 1
        case _ =>
      }
      rec(e)
    }
    override def toString =
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, "+
        s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, "+
        s"other MemberList: $otherObjs"
  }
}
