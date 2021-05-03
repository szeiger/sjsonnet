package sjsonnet

import java.io.StringWriter
import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.mutable

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class OptimizerBenchmark {

  private var inputs: Iterable[(Expr, FileScope)] = _
  private var allFiles: IndexedSeq[(Path, String)] = _
  private var ev: EvalScope = _

  @Setup
  def setup(): Unit = {
    val (allFiles, ev) = MainBenchmark.findFiles()
    this.inputs = allFiles.map { case (p, s) =>
      fastparse.parse(s, new Parser(p).document(_)) match {
        case Success(v, _) => v
      }
    }
    this.ev = ev
    val static = inputs.map {
      case (expr, fs) => ((new StaticOptimizer(ev)).optimize(expr), fs)
    }
    val countBefore, countStatic = new Counter
    inputs.foreach(t => assert(countBefore.transform(t._1) eq t._1))
    static.foreach(t => assert(countStatic.transform(t._1) eq t._1))
    System.err.println(s"Documents: total=${inputs.size}")
    System.err.println(s"Before: $countBefore")
    System.err.println(s"Static: $countStatic")
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (expr, fs) =>
      bh.consume((new StaticOptimizer(ev)).optimize(expr))
    })
  }

  class Counter extends ExprTransform {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
      otherObjs, namedApplies, applies, arityApplies, builtin, closures, nonClosures = 0
    val applyArities = new mutable.LongMap[Int]()
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
        case f: Expr.Function => if(f.closure) closures += 1 else nonClosures += 1
        case e: Expr.ObjBody.MemberList =>
          if(e.binds == null && e.asserts == null && e.fields.forall(_.isStatic)) missedStaticObjs += 1
          else otherObjs += 1
        case e: Expr.Apply =>
          if(e.namedNames == null) {
            applies += 1
            val a = e.args.length
            applyArities.put(a.toLong, applyArities.getOrElse(a.toLong, 0) + 1)
          } else namedApplies += 1

        case _: Expr.Apply0 | _: Expr.Apply1 | _: Expr.Apply2 | _: Expr.Apply3 => arityApplies += 1
        case _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin1 | _: Expr.ApplyBuiltin2 => builtin += 1
        case _ =>
      }
      rec(e)
    }
    override def transformBind(b: Expr.Bind): Expr.Bind = {
      if(b.closure) closures += 1 else nonClosures += 1
      super.transformBind(b)
    }
    override def transformField(f: Expr.Member.Field): Expr.Member.Field = {
      if(f.closure) closures += 1 else nonClosures += 1
      super.transformField(f)
    }
    override def toString = {
      val arities = applyArities.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, "+
        s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, "+
        s"other MemberList: $otherObjs, named Apply: $namedApplies, other Apply: $applies, "+
        s"ApplyN: $arityApplies, ApplyBuiltin*: $builtin; Apply arities: {$arities}, "+
        s"Closures: $closures, Non-closures: $nonClosures"
    }
  }
}
