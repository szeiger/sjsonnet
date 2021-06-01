package sjsonnet

import sjsonnet.Expr.ObjBody.{MemberList, ObjComp}
import sjsonnet.Expr._

import scala.collection.immutable.HashMap

class ScopedExprTransform extends ExprTransform {
  import ScopedExprTransform._
  var scope: Scope = emptyScope

  // Marker for Exprs in the scope that should not be used because they need to be evaluated in a different scope
  val dynamicExpr = new Expr { def pos: Position = ???; override def toString = "dynamicExpr" }

  def transform(e: Expr): Expr = e match {
    case LocalExpr(pos, bindings, returned) =>
      val (b2, r2) = nestedConsecutiveBindings(bindings)(transformBind)(transform(returned))
      if((b2 eq bindings) && (r2 eq returned)) e
      else LocalExpr(pos, b2, r2)

    case MemberList(pos, binds, fields, asserts, selfSym, superSym) =>
      val fields2 = transformGenericArr(fields)(transformFieldNameOnly)
      val (binds2, (fields3, asserts2)) = nestedObject(selfSym, dynamicExpr, superSym, dynamicExpr) {
        nestedConsecutiveBindings(binds)(transformBind) {
          val fields3 = transformGenericArr(fields2)(transformFieldNoName)
          val asserts2 = transformAsserts(asserts)
          (fields3, asserts2)
        }
      }
      if((binds2 eq binds) && (fields3 eq fields) && (asserts2 eq asserts)) e
      else ObjBody.MemberList(pos, binds2, fields3, asserts2, selfSym, superSym)

    case Function(pos, params, body) =>
      nestedSymbols(params.symbols)(rec(e))

    case ObjComp(pos, preLocals, key, value, postLocals, first, rest, selfSym, superSym) =>
      val (f2 :: r2, (k2, (pre2, post2, v2))) = compSpecs(first :: rest, { () =>
        (transform(key), nestedBindings(selfSym, dynamicExpr, superSym, dynamicExpr, preLocals ++ postLocals) {
          (transformBinds(preLocals), transformBinds(postLocals), transform(value))
        })
      })
      if((f2 eq first) && (k2 eq key) && (v2 eq value) && (pre2 eq preLocals) && (post2 eq postLocals) && (r2, rest).zipped.forall(_ eq _)) e
      else ObjComp(pos, pre2, k2, v2, post2, f2.asInstanceOf[ForSpec], r2, selfSym, superSym)

    case Comp(pos, value, first, rest) =>
      val (f2 :: r2, v2) = compSpecs(first :: rest.toList, () => transform(value))
      if((f2 eq first) && (v2 eq value) && (r2, rest).zipped.forall(_ eq _)) e
      else Comp(pos, v2, f2.asInstanceOf[ForSpec], r2.toArray)

    case e => rec(e)
  }

  override def transformBind(b: Bind): Bind = {
    val args = b.args
    val rhs = b.rhs
    nestedSymbols(if(args == null) null else args.symbols) {
      val args2 = transformParams(args)
      val rhs2 = transform(rhs)
      if((args2 eq args) && (rhs2 eq rhs)) b
      else b.copy(args = args2, rhs = rhs2)
    }
  }

  protected[this] def transformFieldNameOnly(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val x2 = transformFieldName(x)
    if(x2 eq x) f else f.copy(fieldName = x2)
  }

  protected[this] def transformFieldNoName(f: Member.Field): Member.Field = {
    def g = {
      val y = f.args
      val z = f.rhs
      val y2 = transformParams(y)
      val z2 = transform(z)
      if((y2 eq y) && (z2 eq z)) f else f.copy(args = y2, rhs = z2)
    }
    if(f.args == null) g
    else nestedSymbols(f.args.symbols)(g)
  }

  override protected[this] def transformField(f: Member.Field): Member.Field = ???

  protected[this] def compSpecs[T](a: List[CompSpec], value: () => T): (List[CompSpec], T) = a match {
    case (c @ ForSpec(pos, name, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[ForSpec]
      nestedWith(c2.sym, dynamicExpr) {
        val (cs2, value2) = compSpecs(cs, value)
        (c2 :: cs2, value2)
      }
    case (c @ IfSpec(pos, cond)) :: cs =>
      val c2 = rec(c).asInstanceOf[CompSpec]
      val (cs2, value2) = compSpecs(cs, value)
      (c2 :: cs2, value2)
    case Nil =>
      (Nil, value())
  }

  protected[this] def nestedNew[T](sc: Scope)(f: => T): T = {
    val oldScope = scope
    scope = sc
    try f finally { scope = oldScope }
  }

  protected[this] def nestedWith[T](s: Symbol, e: Expr)(f: => T): T =
    nestedNew(new Scope(scope.mappings.updated(s.name, new ScopedVal(e, scope, scope.size, s)), scope.size+1))(f)

  protected[this] def nestedFileScope[T](fs: FileScope)(f: => T): T =
    nestedNew(emptyScope)(f)

  protected[this] def nestedConsecutiveBindings[T](a: Array[Bind])(f: => Bind => Bind)(g: => T): (Array[Bind], T) = {
    if(a == null || a.length == 0) (a, g)
    else {
      val oldScope = scope
      try {
        val mappings = a.zipWithIndex.map { case (b, idx) =>
          (b.sym.name, new ScopedVal(if(b.args == null) b.rhs else b, scope, scope.size + idx, b.sym))
        }
        scope = new Scope(oldScope.mappings ++ mappings, oldScope.size + a.length)
        var changed = false
        val a2 = a.zipWithIndex.map { case (b, idx) =>
          val b2 = f(b)
          val sv = mappings(idx)._2.copy(v = if(b2.args == null) b2.rhs else b2)
          scope = new Scope(scope.mappings.updated(b.sym.name, sv), scope.size)
          if(b2 ne b) changed = true
          b2
        }
        (if(changed) a2 else a, g)
      } finally { scope = oldScope }
    }
  }

  protected[this] def nestedBindings[T](a: Array[Bind])(f: => T): T = {
    if(a == null || a.length == 0) f
    else {
      val newm = a.zipWithIndex.map { case (b, idx) =>
        //println(s"Binding ${b.name} to ${scope.size + idx}")
        (b.sym.name, new ScopedVal(if(b.args == null) b.rhs else b, scope, scope.size + idx, b.sym))
      }
      nestedNew(new Scope(scope.mappings ++ newm, scope.size + a.length))(f)
    }
  }

  protected[this] def nestedObject[T](selfSym: Symbol, self0: Expr, superSym: Symbol, super0: Expr)(f: => T): T = {
    val self = new ScopedVal(self0, scope, scope.size, selfSym)
    val sup = new ScopedVal(super0, scope, scope.size+1, superSym)
    val newm = {
      val m1 = scope.mappings + (("self", self)) + (("super", sup))
      if(scope.contains("self")) m1 else m1 + (("$", self))
    }
    nestedNew(new Scope(newm, scope.size + 2))(f)
  }

  protected[this] def nestedBindings[T](selfSym: Symbol, self0: Expr, superSym: Symbol, super0: Expr, a: Array[Bind])(f: => T): T =
    nestedObject(selfSym, self0, superSym, super0)(nestedBindings(a)(f))

  protected[this] def nestedSymbols[T](a: Array[Symbol])(f: => T): T = {
    if(a == null || a.length == 0) f
    else {
      val newm = a.zipWithIndex.map { case (s, idx) => (s.name, new ScopedVal(dynamicExpr, scope, scope.size + idx, s)) }
      nestedNew(new Scope(scope.mappings ++ newm, scope.size + a.length))(f)
    }
  }
}

object ScopedExprTransform {
  final case class ScopedVal(v: AnyRef, sc: Scope, idx: Int, sym: Symbol)
  final class Scope(val mappings: HashMap[String, ScopedVal], val size: Int) {
    def get(s: String): ScopedVal = mappings.getOrElse(s, null)
    def contains(s: String): Boolean = mappings.contains(s)
  }
  def emptyScope: Scope = new Scope(HashMap.empty, 0)
}
