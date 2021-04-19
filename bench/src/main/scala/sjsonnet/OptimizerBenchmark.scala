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

  private var inputs: Iterable[(Expr, FileScope)] = _
  private var allFiles: IndexedSeq[(Path, String)] = _

  @Setup
  def setup(): Unit = {
    val allFiles = MainBenchmark.findFiles()
    this.inputs = allFiles.map { case (p, s) =>
      fastparse.parse(s, new Parser(p).document(_)) match {
        case Success(v, _) => v
      }
    }
    val static = inputs.map {
      case (expr, fs) => ((new StaticOptimizer()(null)).optimize(expr), fs)
    }
    val countBefore, countStatic = new Counter
    inputs.foreach(t => assert(countBefore.transform(t._1) eq t._1))
    static.foreach(t => assert(countStatic.transform(t._1) eq t._1))
    System.err.println(s"Documents: total=${inputs.size}")
    System.err.println(s"Before: $countBefore")
    System.err.println(s"Static: $countStatic")
    val cf = new ClosureFinder
    static.foreach(t => cf.transform(t._1))
    System.err.println(cf)
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (expr, fs) =>
      bh.consume((new StaticOptimizer()(null)).optimize(expr))
    })
  }

  class Counter extends ExprTransform {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
      otherObjs, applies, builtin = 0
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
        case _: Expr.Apply => applies += 1
        case _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin1 | _: Expr.ApplyBuiltin2 => builtin += 1
        case _ =>
      }
      rec(e)
    }
    override def toString =
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, "+
        s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, "+
        s"other MemberList: $otherObjs, Apply: $applies, ApplyBuiltin*: $builtin"
  }

  class ClosureFinder extends ScopedExprTransform {
    var closures, potentialClosures, others, totalDefSize, totalUsedSize = 0
    override def transform(e: Expr): Expr = e match {
      case Expr.Function(pos, params, body, closure) =>
        val min = (new ClosureCounter).findMin(e, scope)
        if(min >= scope.size) {
          if(closure) closures += 1
          else potentialClosures += 1
        } else others += 1
        totalDefSize += scope.size
        totalUsedSize += math.max(0, scope.size-min)
        super.transform(e)
//      case Expr.LocalExpr(pos, bindings, returned) =>
//        val sclen = scope.size
//        nestedBindings(bindings) {
//
//        }
//        super.transform(e)
      case e => super.transform(e)
    }
    override def transformBind(b: Expr.Bind): Expr.Bind = {
      if(b.args != null) {
        val min = (new ClosureCounter).findMin(b, scope)
        if(min >= scope.size) {
          if(b.closure) closures += 1
          else potentialClosures += 1
        }
        else others += 1
      }
      super.transformBind(b)
    }
    override def transformFieldNoName(f: Expr.Member.Field): Expr.Member.Field = {
      if(f.args != null) {
        val min = (new ClosureCounter).findMin(f, scope)
        if(min >= scope.size) {
          if(f.closure) closures += 1
          else potentialClosures += 1
        }
        else others += 1
      }
      super.transformFieldNoName(f)
    }
    override def toString = {
      val avgDef = totalDefSize.toDouble / (closures + others)
      val avgUsed = totalUsedSize.toDouble / (closures + others)
      s"Closures: $closures, potential: $potentialClosures, other functions: $others, avg fun scope: $avgDef, "+
        s"avg used scope: $avgUsed"
    }
  }

  class ClosureCounter extends ScopedExprTransform {
    var minIdx = Int.MaxValue
    def findMin(e: Expr, sc: ScopedExprTransform.Scope): Int = {
      nestedNew(sc)(transform(e))
      minIdx
    }
    def findMin(b: Expr.Bind, sc: ScopedExprTransform.Scope): Int = {
      nestedNew(sc)(transformBind(b))
      minIdx
    }
    def findMin(b: Expr.Member.Field, sc: ScopedExprTransform.Scope): Int = {
      nestedNew(sc)(transformFieldNoName(b))
      minIdx
    }
    override def transform(e: Expr): Expr = e match {
      case Expr.ValidId(_, deBrujin, _) =>
        val idx = scope.size - deBrujin
        if(idx < minIdx) minIdx = idx
        e
      case e => super.transform(e)
    }
  }
}
