package sjsonnet

import Expr._

abstract class ExprTransform {

  def transform(expr: Expr): Expr

  def rec(expr: Expr): Expr = {
    expr match {
      case Select(pos, x, name) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else Select(pos, x2, name)

      case Apply(pos, x, y, namedNames) =>
        val x2 = transform(x)
        val y2 = transformArr(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Apply(pos, x2, y2, namedNames)

      case ApplyBuiltin(pos, func, x) =>
        val x2 = transformArr(x)
        if(x2 eq x) expr
        else ApplyBuiltin(pos, func, x2)

      case ApplyBuiltin1(pos, func, x) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else ApplyBuiltin1(pos, func, x2)

      case ApplyBuiltin2(pos, func, x, y) =>
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else ApplyBuiltin2(pos, func, x2, y2)

      case UnaryOp(pos, op, x) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else UnaryOp(pos, op, x2)

      case BinaryOp(pos, x, op, y) =>
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else BinaryOp(pos, x2, op, y2)

      case Lookup(pos, x, y) =>
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Lookup(pos, x2, y2)

      case Function(pos, x, y, closure) =>
        val x2 = transformParams(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else Function(pos, x2, y2, closure)

      case LocalExpr(pos, x, y) =>
        val x2 = transformBinds(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else LocalExpr(pos, x2, y2)

      case IfElse(pos, x, y, z) =>
        val x2 = transform(x)
        val y2 = transform(y)
        val z2 = transform(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else IfElse(pos, x2, y2, z2)

      case ObjBody.MemberList(pos, x, y, z) =>
        val x2 = transformBinds(x)
        val y2 = transformFields(y)
        val z2 = transformAsserts(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else ObjBody.MemberList(pos, x2, y2, z2)

      case AssertExpr(pos, x, y) =>
        val x2 = transformAssert(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else AssertExpr(pos, x2, y2)

      case Comp(pos, x, y, z) =>
        val x2 = transform(x)
        val y2 = transform(y).asInstanceOf[ForSpec]
        val z2 = transformArr(z)
        if((x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else Comp(pos, x2, y2, z2)

      case Arr(pos, x) =>
        val x2 = transformArr(x)
        if(x2 eq x) expr
        else Arr(pos, x2)

      case ObjExtend(superPos, x, y) =>
        val x2 = transform(x)
        val y2 = transform(y)
        if((x2 eq x) && (y2 eq y)) expr
        else ObjExtend(superPos, x2, y2.asInstanceOf[ObjBody])

      case ObjBody.ObjComp(pos, p, k, v, o, f, r) =>
        val p2 = transformBinds(p)
        val k2 = transform(k)
        val v2 = transform(v)
        val o2 = transformBinds(o)
        val f2 = transform(f).asInstanceOf[ForSpec]
        val r2 = transformList(r).asInstanceOf[List[CompSpec]]
        if((p2 eq p) && (k2 eq k) && (v2 eq v) && (o2 eq o) && (f2 eq f) && (r2 eq r)) expr
        else ObjBody.ObjComp(pos, p2, k2, v2, o2, f2, r2)

      case Slice(pos, v, x, y, z) =>
        val v2 = transform(v)
        val x2 = transformOption(x)
        val y2 = transformOption(y)
        val z2 = transformOption(z)
        if((v2 eq v) && (x2 eq x) && (y2 eq y) && (z2 eq z)) expr
        else Slice(pos, v2, x2, y2, z2)

      case IfSpec(pos, x) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else IfSpec(pos,  x2)

      case ForSpec(pos, name, x) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else ForSpec(pos, name, x2)

      case Error(pos, x) =>
        val x2 = transform(x)
        if(x2 eq x) expr
        else Error(pos, x2)

      case other => other
    }
  }

  def transformArr[T <: Expr](a: Array[T]): Array[T] =
    transformGenericArr(a)((transform _).asInstanceOf[T => T])

  def transformParams(p: Params): Params = {
    if(p == null) return null
    val defs = p.defaultExprs
    if(defs == null) p
    else {
      val defs2 = transformArr(defs)
      if(defs2 eq defs) p
      else p.copy(defaultExprs = defs2)
    }
  }

  def transformBinds(a: Array[Bind]): Array[Bind] =
    transformGenericArr(a)(transformBind)

  def transformFields(a: Array[Member.Field]): Array[Member.Field] =
    transformGenericArr(a)(transformField)

  def transformAsserts(a: Array[Member.AssertStmt]): Array[Member.AssertStmt] =
    transformGenericArr(a)(transformAssert)

  def transformBind(b: Bind): Bind = {
    val args = b.args
    val rhs = b.rhs
    val args2 = transformParams(args)
    val rhs2 = transform(rhs)
    if((args2 eq args) && (rhs2 eq rhs)) b
    else b.copy(args = args2, rhs = rhs2)
  }

  def transformField(f: Member.Field): Member.Field = {
    val x = f.fieldName
    val y = f.args
    val z = f.rhs
    val x2 = transformFieldName(x)
    val y2 = transformParams(y)
    val z2 = transform(z)
    if((x2 eq x) && (y2 eq y) && (z2 eq z)) f
    else f.copy(fieldName = x2, args = y2, rhs = z2)
  }

  def transformFieldName(f: FieldName): FieldName = f match {
    case FieldName.Dyn(x) =>
      val x2 = transform(x)
      if(x2 eq x) f else FieldName.Dyn(x2)
    case _ => f
  }

  def transformAssert(a: Member.AssertStmt): Member.AssertStmt = {
    val x = a.value
    val y = a.msg
    val x2 = transform(x)
    val y2 = transform(y)
    if((x2 eq x) && (y2 eq y)) a
    else a.copy(value = x2, msg = y2)
  }

  def transformOption(o: Option[Expr]): Option[Expr] = o match {
    case Some(e) =>
      val e2 = transform(e)
      if(e2 eq e) o else Some(e2)
    case None => o
  }

  def transformList(l: List[Expr]): List[Expr] = {
    val lb = List.newBuilder[Expr]
    var diff = false
    l.foreach { e =>
      val e2 = transform(e)
      lb.+=(e2)
      if(e2 ne e) diff = true
    }
    if(diff) lb.result() else l
  }

  def transformGenericArr[T <: AnyRef](a: Array[T])(f: T => T): Array[T] = {
    if(a == null) return null
    var i = 0
    while(i < a.length) {
      val x1 = a(i)
      val x2 = f(x1)
      if(x1 ne x2) {
        val a2 = a.clone()
        a2(i) = x2
        i += 1
        while(i < a2.length) {
          a2(i) = f(a2(i))
          i += 1
        }
        return a2
      }
      i += 1
    }
    a
  }
}
