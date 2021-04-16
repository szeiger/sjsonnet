package sjsonnet

import java.util

import scala.jdk.CollectionConverters._

import Expr._

class StrictnessAnalyzer(rootFileScope: FileScope, materializeRoot: Boolean) extends ScopedExprTransform(rootFileScope) {
  val strict = new util.IdentityHashMap[Expr, java.lang.Boolean]()
  var materialize = materializeRoot

  def strictImports = strict.keySet().asScala.collect { case i: Import => i }

  override def transform(e: Expr): Expr = {
    if(!strict.containsKey(e)) {
      strict.put(e, true)
      e match {
        case BinaryOp(pos, Val.True(_), Expr.BinaryOp.OP_&&, rhs) =>
          rec(e)

        case BinaryOp(pos, Val.False(_), Expr.BinaryOp.OP_||, rhs) =>
          rec(e)

        case BinaryOp(pos, lhs, Expr.BinaryOp.OP_&&, rhs) =>
          transform(lhs)
          e

        case BinaryOp(pos, lhs, Expr.BinaryOp.OP_||, rhs) =>
          transform(lhs)
          e

        case BinaryOp(pos, lhs, Expr.BinaryOp.OP_in, rhs) =>
          nomat(transform(lhs))
          transform(rhs)
          e

        case _: Val.Literal | _: $ | _: Self | _: UnaryOp | _: BinaryOp | _: Error | _: Select =>
          rec(e)

        case _: Slice | _: Lookup | _: ApplyBuiltin1 | _: ApplyBuiltin2 | _: ApplyBuiltin =>
          nomat(rec(e))

        case ObjExtend(pos, base, ext) =>
          nomat(transform(base))
          transform(ext)
          e

        case Id(pos, value) =>
          scope(value) match {
            case ScopedVal(ex: Expr, sc) if ex ne dynamicExpr =>
              nested(sc)(transform(ex))
            case ScopedVal(b: Bind, sc) if materialize =>
              nested(sc)(nestedIndices(b.args.indices)(transform(b.rhs)))
            case _ =>
          }
          e

        case Apply(pos, value, argNames, argExprs) =>
          nomat(transform(value))
          e

        case LocalExpr(pos, bindings, returned) =>
          nestedBindings(bindings)(transform(returned))
          e

        case IfElse(pos, cond, then0, else0) =>
          transform(cond) match {
            case Val.True(_) => transform(then0)
            case Val.False(_) => transform(else0)
            case _ =>
          }
          e

        case ObjBody.MemberList(pos, binds, fields, asserts) =>
          transformGenericArr(fields)(transformFieldNameOnly)
          if(materialize) {
            nestedBindings(binds) {
              transformGenericArr(fields)(transformFieldNoName)
            }
          }
          e

        case Arr(pos, value) if materialize =>
          transformArr(value)
          e

        case AssertExpr(pos, Member.AssertStmt(value, msg), returned) =>
          transform(value) match {
            case Val.True(_) =>
            case _: Val => transform(msg)
            case _ =>
          }
          transform(returned)
          e

        case Comp(pos, value, first, rest) =>
          transform(first)
          e

        //TODO: case ResolvedImport(pos, path, expr, fs) => visitResolvedImport(pos, path, expr, fs)

        case e => e
      }
    } else e
  }

  def nomat[T](f: => T): T = {
    val m = materialize
    materialize = false
    try f finally materialize = m
  }

  override protected[this] def transformFieldNoName(f: Member.Field): Member.Field = {
    def g = {
      nomat(transformParams(f.args))
      transform(f.rhs)
    }
    nestedIndices(f.args.indices)(g)
    f
  }
}
