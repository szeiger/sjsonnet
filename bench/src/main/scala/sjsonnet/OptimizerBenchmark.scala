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
    val rc = new RefCounter
    val rr = new RefReporter(rc)
    static.foreach(t => rc.transform(t._1))
    static.foreach(t => rr.transform(t._1))
    System.err.println(rr)
  }

  @Benchmark
  def main(bh: Blackhole): Unit = {
    bh.consume(inputs.foreach { case (expr, fs) =>
      bh.consume((new StaticOptimizer(ev)).optimize(expr))
    })
  }

  class Counter extends ExprTransform {
    var total, vals, exprs, arrVals, staticArrExprs, otherArrExprs, staticObjs, missedStaticObjs,
      otherObjs, namedApplies, applies, arityApplies, builtin = 0
    val applyArities = new mutable.LongMap[Int]()
    val ifElseChains = new mutable.LongMap[Int]()
    val selectChains = new mutable.LongMap[Int]()
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
      val ifElseCount = countIfElse(e)
      if(ifElseCount > 0) {
        ifElseChains.put(ifElseCount.toLong, ifElseChains.getOrElse(ifElseCount.toLong, 0) + 1)
        if(ifElseCount > 1)
          ifElseChains.put(ifElseCount.toLong-1L, ifElseChains.getOrElse(ifElseCount.toLong-1L, 0) - 1)
      }
      val selectCount = countSelectOnId(e)
      if(selectCount >= 0) {
        selectChains.put(selectCount.toLong, selectChains.getOrElse(selectCount.toLong, 0) + 1)
        if(selectCount > 0)
          selectChains.put(selectCount.toLong-1L, selectChains.getOrElse(selectCount.toLong-1L, 0) - 1)
      }
      rec(e)
    }
    def countIfElse(e: Expr): Int = e match {
      case Expr.IfElse(_, _, _, else0) =>
        countIfElse(else0) + 1
      case _ => 0
    }
    def countSelectOnId(e: Expr): Int = e match {
      case Expr.Select(_, x, _) =>
       val c = countSelectOnId(x)
        if(c == -1) -1 else c + 1
      case _: Expr.ValidId => 0
      case _ => -1
    }
    override def toString = {
      val arities = applyArities.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      val chains = ifElseChains.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      val selChains = selectChains.toSeq.sortBy(_._1).map { case (a,b) => s"$a: $b" }.mkString(", ")
      s"Total: $total, Val: $vals, Expr: $exprs, Val.Arr: $arrVals, static Expr.Arr: $staticArrExprs, "+
        s"other Expr.Arr: $otherArrExprs, Val.Obj: $staticObjs, static MemberList: $missedStaticObjs, "+
        s"other MemberList: $otherObjs, named Apply: $namedApplies, other Apply: $applies, "+
        s"ApplyN: $arityApplies, ApplyBuiltin*: $builtin; Apply arities: {$arities}, "+
        s"if/else chains: $chains, Select/ValidId chains: $selChains"
    }
  }

  class RefCounter extends ScopedExprTransform {
    final class Def(val idx: Int, var all: Int = 0, var safe: Int = 0)

    val defsByBind = new java.util.IdentityHashMap[Expr.Bind, Def]
    private var safe = true

    override def transform(e: Expr): Expr = e match {
      case Expr.LocalExpr(_, bs, _) =>
        bs.zipWithIndex.foreach { case (b, i) =>
          val key = scope.size+i
          //assert(!defsByBind.containsKey(b))
          if(!defsByBind.containsKey(b)) {
            val d = new Def(key)
            defsByBind.put(b, d)
          }
        }
        super.transform(e)
      case Expr.ObjBody.MemberList(_, bs, _, _) if bs != null =>
        bs.zipWithIndex.foreach { case (b, i) =>
          val key = scope.size+2+i
          //assert(!defsByBind.containsKey(b), s"unexpected duplicate Bind: $b")
          if(!defsByBind.containsKey(b)) {
            val d = new Def(key)
            defsByBind.put(b, d)
          }
        }
        super.transform(e)
      case Expr.ValidId(_, name, idx) =>
        val sv = scope.get(name)
        if(sv != null && sv.bind != null) {
          defsByBind.get(sv.bind) match {
            case null =>
            case d =>
              assert(d.idx == idx)
              d.all += 1
              if(safe) d.safe += 1
          }
        }
        super.transform(e)
      case _: Expr.Comp | _: Expr.ObjBody.ObjComp => //TODO treat first generator as safe
        val prevSafe = safe
        safe = false
        val res = super.transform(e)
        safe = prevSafe
        res
      case _ => super.transform(e)
    }
  }

  class RefReporter(rc: RefCounter) extends ScopedExprTransform {
    var ref0, ref1safeval, ref1safefunc, ref1, refMore = 0

    private def count(b: Expr.Bind): Unit = {
      val d = rc.defsByBind.get(b)
      d.all match {
        case 0 => ref0 += 1
          //System.err.println(s"Found 0 refs for $b")
        case 1 if d.safe == 1 && b.args == null => ref1safeval += 1
        case 1 if d.safe == 1 => ref1safefunc += 1
        case 1 => ref1 += 1
        case _ => refMore += 1
      }
    }

    override def transform(e: Expr): Expr = e match {
      case Expr.LocalExpr(_, bs, _) =>
        bs.foreach(count)
        super.transform(e)
      case Expr.ObjBody.MemberList(_, bs, _, _) if bs != null =>
        bs.foreach(count)
        super.transform(e)
      case _ => super.transform(e)
    }

    override def toString = {
      s"Ref Counts: 0 refs -> $ref0, val with 1 safe ref: $ref1safeval, func with 1 safe ref: $ref1safefunc, 1 unsafe ref -> $ref1, more refs: $refMore"
    }
  }
}
