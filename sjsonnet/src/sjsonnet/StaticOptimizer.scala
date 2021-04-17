package sjsonnet

import Expr._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class StaticOptimizer(rootFileScope: FileScope)(implicit eval: EvalErrorScope)
  extends ScopedExprTransform(rootFileScope) {

  override def transform(e: Expr): Expr = e match {
    case Apply(pos, Select(_, Id(_, 0), name), null, args) if(scope(0) == null) =>
      //println(s"----- std.$name(#${args.length}) call")
      Std.functions.getOrElse(name, null) match {
        case f: Val.Builtin =>
          val rargs = transformArr(args)
          val alen = rargs.length
          f match {
            case f: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f, rargs(0))
            case f: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f, rargs(0), rargs(1))
            case _ if f.params.indices.length == alen => Expr.ApplyBuiltin(pos, f, rargs)
            case _ => rec(e)
          }
        case _ => rec(e)
      }

    case Id(pos, name) =>
      val v = scope(name)
      v match {
        case ScopedVal(v: Val with Expr, _) =>
          //println(s"----- Id($pos, $name) -> $v")
          v
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
        case m @ ObjBody.MemberList(pos, backdrop, binds, fields, asserts) =>
          if(binds == null && asserts == null && fields.forall(_.isStatic) && (backdrop == null || isStatic(backdrop)))
            Val.staticObject(pos, fields, backdrop)
          else if(fields.forall(_.fieldName.isInstanceOf[Expr.FieldName.Fixed])) {
            val backdrop = new java.util.LinkedHashMap[String,Val.Obj.Member](fields.length*3/2)
            val rest = new mutable.ArrayBuilder.ofRef[Expr.Member.Field]
            fields.foreach { f =>
              val n = f.fieldName.asInstanceOf[Expr.FieldName.Fixed].value
              f.rhs match {
                case v: Val if f.args == null =>
                  val m = new Val.Obj.ConstMember(f.plus, f.sep, v, true)
                  backdrop.put(n, m)
                case _ =>
                  backdrop.put(n, null)
                  rest.+=(f)
              }
            }
            //if(fields.length-rest.length != 0)
            //  println(s"creating backdrop for ${fields.length-rest.length} of ${fields.length} in $m")
            new Expr.ObjBody.MemberList(pos, if(rest.length == fields.length) null else backdrop, binds, rest.result(), asserts)
          } else m
        case other => other
      }

    case e => super.transform(e)
  }

  def isStatic(m: java.util.LinkedHashMap[String,Val.Obj.Member]): Boolean = {
    m.asScala.forall { case (k, m) => m.isInstanceOf[Val.Obj.ConstMember] && !m.add && m.visibility == Expr.Member.Visibility.Normal }
  }

  override protected[this] def transformFieldName(f: FieldName): FieldName = f match {
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
}
