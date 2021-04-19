package sjsonnet

import java.util

import Expr._
import ScopedExprTransform._

import scala.util.control.Breaks

class StaticOptimizer(implicit eval: EvalErrorScope)
  extends ScopedExprTransform {

  private val closureId = new ClosureIdentifyer

  def optimize(root: Expr): Expr = {
    (new ClosureInliner).transform(transform(root))
  }

  override def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, "std"), name), args, null) if(scope.get("std") == null) =>
      //println(s"----- std.$name(#${args.length}) call")
      Std.functions.getOrElse(name, null) match {
        case f: Val.Builtin =>
          val rargs = transformArr(args)
          val alen = rargs.length
          f match {
            case f: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f, rargs(0))
            case f: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f, rargs(0), rargs(1))
            case _ if f.params.names.length == alen => Expr.ApplyBuiltin(pos, f, rargs)
            case _ => rec(e)
          }
        case _ => rec(e)
      }

    case Select(_, Id(_, "std"), name) if(scope.get("std") == null) =>
      Std.functions.getOrElse(name, null) match {
        case null => rec(e)
        case f => f
      }

    case Id(pos, name) =>
      val v = scope.get(name)
      v match {
        case ScopedVal(v: Val with Expr, _, _) => v
        case ScopedVal(e, _, idx) => ValidId(pos, scope.size-idx, name)
        case null if name == "std" => Std.Std
        case _ => e
      }

    case a: Arr =>
      super.transform(a) match {
        case a: Arr if a.value.forall(_.isInstanceOf[Val]) =>
          new Val.Arr(a.pos, a.value.map(e => new Val.Strict(e.asInstanceOf[Val])))
        case other => other
      }

    case m: ObjBody.MemberList =>
      super.transform(m) match {
        case m @ ObjBody.MemberList(pos, binds, fields, asserts) =>
          if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields)
          else m
        case other => other
      }

    case _: Function =>
      super.transform(e) match {
        case f @ Expr.Function(pos, params, body, false) if closureId.isClosed(scope)(_.transform(f)) =>
          Expr.Function(pos, params, body, true)
        case f => f
      }

    case e => super.transform(e)
  }

  override def transformBind(b: Bind): Bind = {
    val b2 = super.transformBind(b)
    if(b2.args != null && closureId.isClosed(scope)(_.transformBind(b2))) {
      //println(s"--- Bind closure: $b2")
      b2.copy(closure = true)
    } else b2
  }

  override def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      transform(x) match {
        case x2: Val.Str =>
          //println(s"----- Fixing FieldName: "+x2.value)
          FieldName.Fixed(x2.value)
        case x2 if x2 eq x => f
        case x2 => FieldName.Dyn(x2)
      }
    case _ => f
  }

  override def transformFieldNoName(f: Expr.Member.Field): Expr.Member.Field = {
    val f2 = super.transformFieldNoName(f)
    if(f2.args != null && closureId.isClosed(scope)(_.transformFieldNoName(f2))) {
      f2.copy(closure = true)
    } else f2
  }
}

class ClosureIdentifyer extends ScopedExprTransform {
  private var minIdx = Int.MaxValue
  private var target: Int = 0
  private var seenObj: Boolean = false
  def isClosed[T](sc: ScopedExprTransform.Scope)(f: this.type => T): Boolean = {
    minIdx = Int.MaxValue
    target = sc.size
    seenObj = false
    Breaks.breakable(nestedNew(sc)(f(this)))
    minIdx >= target
  }
  override def transform(e: Expr): Expr = e match {
    case Expr.ValidId(_, deBrujin, _) =>
      val idx = scope.size - deBrujin
      if(idx < minIdx) minIdx = idx
      if(minIdx < target) Breaks.break()
      e
    case e: Expr.ObjBody =>
      val b = seenObj
      seenObj = true
      val e2 = super.transform(e)
      seenObj = b
      e2
    case _: Expr.$ =>
      minIdx = -1
      Breaks.break()
    case (_: Expr.Self | _: Expr.Super) if !seenObj =>
      minIdx = -1
      Breaks.break()
    case e => super.transform(e)
  }
}

class ClosureInliner extends ScopedExprTransform {
  private val cache = new util.IdentityHashMap[AnyRef, Expr.Function]()

  override def transform(e: Expr): Expr = super.transform(e) match {
    case e2 @ Expr.ValidId(_, _, name) =>
      scope.get(name).v match {
        case c: Expr.Function if c.closure =>
          c
        case b @ Expr.Bind(pos, name, args, rhs, true) =>
          //println(s"---- Inlining $b")
          var cached = cache.get(b)
          if(cached == null) {
            cached = Expr.Function(pos, args, rhs, true)
            cache.put(b, cached)
          }
          cached
        case _ => e2
      }
    case e2 @ Expr.Apply(_, Expr.Function(_, _, v: Val, _), _, _) =>
      //println(s"----- Inlining constant function $e2")
      v
    case e2 => e2
  }
}
