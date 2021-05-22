package sjsonnet

import java.io.StringWriter
import java.util
import java.util.concurrent.TimeUnit

import fastparse.Parsed.Success
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.control.NonFatal

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
    val (allFiles, interp) = MainBenchmark.findFiles()
    this.inputs = allFiles.map { case (p, s) =>
      fastparse.parse(s, new Parser(p).document(_)) match {
        case Success(v, _) => v
      }
    }
    this.ev = interp.evaluator
    val static = inputs.map {
      case (expr, fs) => ((new StaticOptimizer(ev)).optimize(expr), fs)
    }
    val countBefore, countStatic = new Counter
    inputs.foreach(t => assert(countBefore.transform(t._1) eq t._1))
    static.foreach(t => assert(countStatic.transform(t._1) eq t._1))
    System.err.println(s"Documents: total=${inputs.size}")
    System.err.println(s"Before: $countBefore")
    System.err.println(s"Static: $countStatic")
    val sa = new StrictnessAnalyzer(interp)
    sa.materializeRoot(inputs.head._1)
    sa.materialized.forEach { (e, b) =>
      e match {
        case e: Expr =>
          println(s"Strict (materialized=$b): ${exprStr(e)(ev)}")
        case e =>
          println(s"Strict (materialized=$b): $e")
      }
    }
  }

  def exprStr(expr: Expr)(implicit ev: EvalErrorScope): String = {
    val prettyOffset = ev.prettyIndex(expr.pos).map { case (l,c) => s"$l:$c" }.getOrElse("?:?")
    val prettyPos = s"${expr.pos.currentFile.asInstanceOf[OsPath].p}:$prettyOffset"
    val name = expr.getClass.getName.split('.').last.split('$').last
    s"$name $prettyPos"
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
}

class StrictnessAnalyzer(interp: Interpreter) {
  import ScopedExprTransform.{Scope, ScopedVal}

  val materialized = new util.IdentityHashMap[Expr, Boolean]()
  val evaluated = new util.IdentityHashMap[Expr, Option[Expr]]()
  val scopes = new util.IdentityHashMap[Expr, Scope]()
  val calls = new util.HashMap[Call, Option[Expr]]()
  var argReplacements = HashMap.empty[Expr.Arg, Expr]

  final class Call(val from: Expr, val to: Expr.Params) {
    override def hashCode = System.identityHashCode(from) + System.identityHashCode(to)
    override def equals(obj: Any): Boolean = obj match {
      case c: Call if (from eq c.from) && (to eq c.to) => true
      case _ => false
    }
  }

  def materializeRoot(e: Expr): Unit = {
    println(" ")
    println(" ")
    ScopedExprTransform.buildScopeMap(e, scopes)
    eval(e, 0).foreach(materializeAll(_, 0))
    println(" ")
  }

  def log(msg: String, indent: Int): Unit = {
    val s = (0 until indent*2).map(_ => " ").mkString
    println(s+msg)
  }

  def materializeAll(e: Expr, indent: Int): Unit = if(!materialized.containsKey(e)) {
    log(s"materializeAll: ${exprStr(e)}", indent)
    materialized.put(e, true)
    e match {
      case e @ Expr.ObjBody.MemberList(_, bs, fs, as) =>
        materializeMemberListBase(e, indent+1)
        if(as != null) as.foreach(a => eval(a.value, indent+1))
        fs.foreach(f => eval(f.rhs, indent+1).foreach(materializeAll(_, indent+1)))
      case Expr.Member.Field(_, fn, _, args, _, rhs) =>
        eval(rhs, indent+1).foreach(materializeAll(_, indent+1))
        // TODO handle args strictness
      case Expr.BinaryOp(_, lhs, _, rhs) =>
        eval(lhs, indent+1).foreach(materializeAll(_, indent+1))
        eval(rhs, indent+1).foreach(materializeAll(_, indent+1))
      case _: Val =>
      case Expr.Arr(_, xs) =>
        xs.foreach(x => eval(x, indent+1).foreach(materializeAll(_, indent+1)))
      case e =>
        log(s"*** should materialize ${exprStr(e)}", indent+1)
    }
  }

  def materializeName(e: Expr, name: String, indent: Int): Option[Expr] = {
    log(s"materializeName: $name in ${exprStr(e)}", indent)
    e match {
      case e @ Expr.ObjBody.MemberList(_, bs, fs, as) =>
        materializeMemberListBase(e, indent+1)
        val fo = e.fields.iterator.map { f =>
          val n = f.fieldName match {
            case Expr.FieldName.Dyn(v) => eval(v, indent+1) match {
              case Some(v: Val.Str) => v.value
              case _ => null
            }
            case Expr.FieldName.Fixed(v) => v
          }
          (n, f)
        }.find(_._1 == name).map(_._2)
        fo.map { f => f.rhs }
      case _ =>
        log(s"*** should materialize $name in ${exprStr(e)}", indent+1)
        None // TODO
    }
  }

  def materializeMemberListBase(e: Expr.ObjBody.MemberList, indent: Int): Unit = {
    log(s"materializeMemberListBase", indent)
    if(e.asserts != null) e.asserts.foreach(a => materializeAll(a.value, indent+1))
    e.fields.foreach { f =>
      f.fieldName match {
        case Expr.FieldName.Dyn(v) => materializeAll(v, indent+1)
        case _ =>
      }
    }
  }

  def eval(e: Expr, indent: Int): Option[Expr] = {
    log(s"eval ${exprStr(e)}", indent)
    val res = e match {
      case Expr.LocalExpr(_, b, r) =>
        eval(r, indent+1)
      case e: Expr.ObjBody.MemberList =>
        Some(e)
      case Expr.Apply(_, v, args, _) =>
        eval(v, indent+1)
      // TODO handle args strictness
      case Expr.Select(_, v, n) =>
        eval(v, indent+1).flatMap {
          case e @ Expr.ObjBody.MemberList(_, bs, fs, as) =>
            e.fields.iterator.map { f =>
              val n = f.fieldName match {
                case Expr.FieldName.Dyn(v) => eval(v, indent+1) match {
                  case Some(v: Val.Str) => v.value
                  case _ => null
                }
                case Expr.FieldName.Fixed(v) => v
              }
              (n, f)
            }.find(_._1 == n).map(_._2).flatMap(eval(_, indent+1))
          case _ => None
        }
      case e @ Expr.Import(_, s) =>
        try {
          val (p, str) = interp.resolver.resolveAndReadOrFail(s, e.pos)(interp.evaluator)
          val doc = interp.resolver.parseOrFail(e.pos, s, p, str)(interp.evaluator)._1
          ScopedExprTransform.buildScopeMap(doc, scopes)
          eval(doc, indent+1)
        } catch { case NonFatal(_) => None }
      case Expr.LocalExpr(_, bindings, returned) =>
        eval(returned, indent+1)
      case e: Expr.ObjBody.MemberList =>
        Some(e)
      case e @ Expr.ValidId(_, name, _) =>
        Option(scopes.get(e).get(name)).flatMap { sv =>
          if((sv.v ne ScopedExprTransform.dynamicExpr) && !evaluated.containsKey(sv.v))
            eval(sv.v, indent+1)
          else None
        }
      case Expr.Bind(_, _, args, rhs) =>
        eval(rhs, indent+1)
      case e: Val => Some(e)
      case Expr.BinaryOp(_, lhs, Expr.BinaryOp.OP_+, rhs) =>
        val e1 = eval(lhs, indent+1)
        eval(rhs, indent+1) match {
          case Some(o2: Expr.ObjBody.MemberList) =>
            e1 match {
              case Some(o1: Expr.ObjBody.MemberList) =>
                combine(o1.fields, o2.fields) match {
                  case null => Some(o2)
                  case c => Some(o2.copy(fields = c))
                }
              case _ => Some(o2)
            }
          case Some(o2: Expr.Arr) =>
            e1 match {
              case Some(o1: Expr.Arr) => Some(o2.copy(value = o1.value ++ o2.value))
              case _ => Some(o2)
            }
          case _ => None
        }
      case Expr.BinaryOp(_, lhs, _, rhs) =>
        eval(lhs, indent+1)
        eval(rhs, indent+1)
        None
      case Expr.And(_, lhs, rhs) =>
        eval(lhs, indent+1)
        None
      case Expr.Or(_, lhs, rhs) =>
        eval(lhs, indent+1)
        None
      case Expr.IfElse(_, cond, th, el) =>
        eval(cond, indent+1) match {
          case Some(_: Val.True) => eval(th, indent+1)
          case Some(_: Val.False) => eval(el, indent+1)
          case _ => None
        }
      case Expr.Apply0(_, v) => eval(v, indent+1)
      case e @ Expr.Apply1(_, v, a1) =>
        eval(v, indent+1) match {
          case s @ Some(f: Expr.Function) =>
            log("apply1 on "+exprStr(f), indent+1)
            val c = new Call(e, f.params)
            calls.get(c) match {
              case null =>
                calls.put(c, None)
                val old = argReplacements
                val res = try {
                  argReplacements = argReplacements.updated(f.params.args(0), a1)
                  eval(f.body, indent+1)
                } finally argReplacements = old
                calls.put(c, res)
                log(s"apply1 returned ${res.map(exprStr).getOrElse("None")}", indent+1)
                res
              case res => res
            }
          case s @ Some(f: Expr.Member.Field) if f.args != null =>
            log("apply1 on "+exprStr(f), indent+1)
            val c = new Call(e, f.args)
            calls.get(c) match {
              case null =>
                calls.put(c, None)
                val old = argReplacements
                val res = try {
                  argReplacements = argReplacements.updated(f.args.args(0), a1)
                  eval(f.rhs, indent+1)
                } finally argReplacements = old
                calls.put(c, res)
                log(s"apply1 returned ${res.map(exprStr).getOrElse("None")}", indent+1)
                res
              case res => res
            }
          case s @ Some(f : Expr.Bind) =>
            log("apply1 on "+exprStr(f), indent+1)
            val c = new Call(e, f.args)
            calls.get(c) match {
              case null =>
                calls.put(c, None)
                val old = argReplacements
                val res = try {
                  argReplacements = argReplacements.updated(f.args.args(0), a1)
                  eval(f.rhs, indent+1)
                } finally argReplacements = old
                calls.put(c, res)
                log(s"apply1 returned ${res.map(exprStr).getOrElse("None")}", indent+1)
                res
              case res => res
            }
          case o => o
        }
      case Expr.Apply2(_, v, a1, a2) => eval(v, indent+1)
      case Expr.Apply3(_, v, a1, a2, a3) => eval(v, indent+1)
      case Expr.Apply(_, v, args, _) => eval(v, indent+1)
      case Expr.ApplyBuiltin1(_, _, a1) =>
        eval(a1, indent+1)
        None
      case Expr.ApplyBuiltin2(_, _, a1, a2) =>
        eval(a1, indent+1)
        eval(a2, indent+1)
        None
      case Expr.ApplyBuiltin(_, _, as) =>
        as.foreach(eval(_, indent+1))
        None
      case Expr.Member.Field(_, _, _, null, _, rhs) => eval(rhs, indent+1)
      case a: Expr.Arg if(argReplacements.contains(a)) =>
        eval(argReplacements(a), indent+1)
      case Expr.Comp(_, v, first, rest) =>
        if(evalCompSpec(first :: rest.toList, indent)) eval(v, indent+1)
        else None
      case _: Expr.Arr | _: Expr.Function => Some(e)
      case e =>
        log("*** not evaluated: "+exprStr(e), indent+1)
        Some(e)
    }
    evaluated.put(e, res)
    res
  }

  def evalCompSpec(css: List[Expr.CompSpec], indent: Int): Boolean = {
    css match {
      case Nil => true
      case cs :: rest =>
        cs match {
          case Expr.IfSpec(_, cond) => false
          case Expr.ForSpec(_, _, cond) =>
            eval(cond, indent+1) match {
              case Some(Expr.Arr(_, xs)) if xs.length > 0 =>
                evalCompSpec(rest, indent)
              case _ => false
            }
        }
    }
  }

//  def paramsEvaluated(p: Expr.Params): Seq[(String, Boolean)] = {
//    p
//  }

  def exprStr(o: Expr): String = o match {
    case a: Expr.Arg =>
      s"Arg(${a.name}, ${a.idx})"
    case expr: Expr =>
      val prettyOffset = interp.evaluator.prettyIndex(expr.pos).map { case (l,c) => s"$l:$c" }.getOrElse("?:?")
      val prettyPos = s"${expr.pos.currentFile.asInstanceOf[OsPath].p}:$prettyOffset"
      val name = expr.getClass.getName.split('.').last.split('$').last
      s"$name $prettyPos"
    case b: Expr.Bind =>
      val prettyOffset = interp.evaluator.prettyIndex(b.pos).map { case (l,c) => s"$l:$c" }.getOrElse("?:?")
      val prettyPos = s"${b.pos.currentFile.asInstanceOf[OsPath].p}:$prettyOffset"
      val name = b.getClass.getName.split('.').last.split('$').last
      s"$name $prettyPos"
    case f: Expr.Member.Field =>
      val prettyOffset = interp.evaluator.prettyIndex(f.pos).map { case (l,c) => s"$l:$c" }.getOrElse("?:?")
      val prettyPos = s"${f.pos.currentFile.asInstanceOf[OsPath].p}:$prettyOffset"
      val name = f.getClass.getName.split('.').last.split('$').last
      s"$name $prettyPos"
    case o => String.valueOf(o)
  }

  def combine(fs1: Array[Expr.Member.Field], fs2: Array[Expr.Member.Field]): Array[Expr.Member.Field] = {
    if(!fs1.forall(_.fieldName.isInstanceOf[Expr.FieldName.Fixed])) return null
    if(!fs2.forall(_.fieldName.isInstanceOf[Expr.FieldName.Fixed])) return null
    val names2 = fs2.map(_.fieldName.asInstanceOf[Expr.FieldName.Fixed].value).toSet
    fs1.filter(f => !names2.contains(f.fieldName.asInstanceOf[Expr.FieldName.Fixed].value)) ++ fs2
  }

  object ApplyAny {
    def unapply(e: Expr): Option[(Expr, Array[Expr], Array[String])] = e match {
      case Expr.Apply(_, v, as, ns) => Some((v, as, ns))
      case Expr.Apply0(_, v) => Some((v, Array.empty, Array.empty))
      case Expr.Apply1(_, v, a1) => Some((v, Array(a1), Array.empty))
      case Expr.Apply2(_, v, a1, a2) => Some((v, Array(a1, a2), Array.empty))
      case Expr.Apply3(_, v, a1, a2, a3) => Some((v, Array(a1, a2, a3), Array.empty))
      case _ => None
    }
  }
}
