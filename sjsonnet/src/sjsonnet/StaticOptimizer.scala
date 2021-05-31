package sjsonnet

import Expr._
import ScopedExprTransform._

import scala.collection.mutable

class StaticOptimizer(ev: EvalScope) extends ScopedExprTransform {
  def optimize(e: Expr): Expr = {
    val e2 = transform(e)

    val rc = new RefCounter
    rc.transform(e2)
    val in = new Inliner(rc)
    val e3 = in.transform(e2)

    val rc2 = new RefCounter
    rc2.transform(e3)
    val de = new DefEliminator(rc2)
    de.transform(e3)
  }

  override def transform(_e: Expr): Expr = super.transform(_e) match {
    case a: Apply => transformApply(a)

    case e @ Select(p, obj: Val.Obj, name) if obj.containsKey(name) =>
      try obj.value(name, p)(ev).asInstanceOf[Expr] catch { case _: Exception => e }

    case Select(pos, ValidSuper(_, selfIdx), name) =>
      SelectSuper(pos, selfIdx, name)

    case Lookup(pos, ValidSuper(_, selfIdx), index) =>
      LookupSuper(pos, selfIdx, index)

    case b2 @ BinaryOp(pos, lhs, BinaryOp.OP_in, ValidSuper(_, selfIdx)) =>
      InSuper(pos, lhs, selfIdx)
    case b2 @ BinaryOp(pos, lhs: Val.Str, BinaryOp.OP_%, rhs) =>
      try ApplyBuiltin1(pos, new Format.PartialApplyFmt(lhs.value), rhs)
      catch { case _: Exception => b2 }

    case e @ Id(pos, name) =>
      scope.get(name) match {
        case ScopedVal(v: Val with Expr, _, _, _) => v
        case ScopedVal(_, _, idx, _) => ValidId(pos, name, idx)
        case null if name == "std" => Std.Std
        case _ => e
      }

    case e @ Self(pos) =>
      scope.get("self") match {
        case ScopedVal(v, _, idx, _) if v != null => ValidId(pos, "self", idx)
        case _ => e
      }

    case e @ $(pos) =>
      scope.get("$") match {
        case ScopedVal(v, _, idx, _) if v != null => ValidId(pos, "$", idx)
        case _ => e
      }

    case a: Arr if a.value.forall(_.isInstanceOf[Val]) =>
      new Val.Arr(a.pos, a.value.map(e => e.asInstanceOf[Val]))

    case m @ ObjBody.MemberList(pos, binds, fields, asserts) =>
      if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields)
      else m

    case e => e
  }

  object ValidSuper {
    def unapply(s: Super): Option[(Position, Int)] =
      scope.get("self") match {
        case ScopedVal(v, _, idx, _) if v != null => Some((s.pos, idx))
        case _ => None
      }
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

  private def transformApply(a: Apply): Expr = {
    val rebound = rebindApply(a.pos, a.value, a.args, a.namedNames) match {
      case null => a
      case a => a
    }
    rebound match {
      case a2: Apply => specializeApplyArity(a2)
      case e => e
    }
  }

  private def tryStaticApply(pos: Position, f: Val.Builtin, args: Array[Expr]): Expr = {
    if(args.forall(_.isInstanceOf[Val])) {
      val vargs = args.map(_.asInstanceOf[Val])
      try f.apply(vargs, null, pos)(ev).asInstanceOf[Expr] catch { case _: Exception => return null }
    } else null
  }

  private def specializeApplyArity(a: Apply): Expr = {
    if(a.namedNames != null) a
    else a.args.length match {
      case 0 => Apply0(a.pos, a.value)
      case 1 => Apply1(a.pos, a.value, a.args(0))
      case 2 => Apply2(a.pos, a.value, a.args(0), a.args(1))
      case 3 => Apply3(a.pos, a.value, a.args(0), a.args(1), a.args(2))
      case _ => a
    }
  }

  private def rebindApply(pos: Position, lhs: Expr, args: Array[Expr], names: Array[String]): Expr = lhs match {
    case f: Val.Builtin =>
      rebind(args, names, f.params) match {
        case null => null
        case newArgs =>
          tryStaticApply(pos, f, newArgs) match {
            case null =>
              val (f2, rargs) = f.specialize(newArgs) match {
                case null => (f, newArgs)
                case (f2, a2) => (f2, a2)
              }
              val alen = rargs.length
              f2 match {
                case f2: Val.Builtin1 if alen == 1 => Expr.ApplyBuiltin1(pos, f2, rargs(0))
                case f2: Val.Builtin2 if alen == 2 => Expr.ApplyBuiltin2(pos, f2, rargs(0), rargs(1))
                case _ if f2.params.names.length == alen => Expr.ApplyBuiltin(pos, f2, rargs)
                case _ => null
              }
            case e => e
          }
      }

    case ValidId(_, name, nameIdx) =>
      scope.get(name) match {
        case ScopedVal(Function(_, params, _), _, _, _) =>
          rebind(args, names, params) match {
            case null => null
            case newArgs => Apply(pos, lhs, newArgs, null)
          }
        case ScopedVal(Bind(_, _, params, _), _, _, _) =>
          rebind(args, names, params) match {
            case null => null
            case newArgs => Apply(pos, lhs, newArgs, null)
          }
        case _ => null
      }

    case _ => null
  }

  private def rebind(args: Array[Expr], argNames: Array[String], params: Params): Array[Expr] = {
    if(args.length == params.names.length && argNames == null) return args
    if(args.length > params.names.length) return null // too many args
    val positional = if(argNames != null) args.length - argNames.length else args.length
    val target = new Array[Expr](params.names.length)
    System.arraycopy(args, 0, target, 0, positional)
    if(argNames != null) {
      var i = 0
      var j = args.length - argNames.length
      while(i < argNames.length) {
        val pos = params.paramMap.getOrElse(argNames(i), -1)
        if(pos == -1) return null // unknown arg name
        if(target(pos) != null) return null // duplicate arg
        target(pos) = args(j)
        i += 1
        j += 1
      }
    }
    var i = positional
    while(i < target.length) {
      if(target(i) == null) {
        params.defaultExprs(i) match {
          case v: Val with Expr => target(i) = v
          case _ => return null // no default or non-constant
        }
      }
      i += 1
    }
    target
  }
}

class RefCounter extends ScopedExprTransform {
  final class Def(val idx: Int, var all: Int = 0, var safe: Int = 0)

  val defsByBind = new java.util.IdentityHashMap[Bind, Def]
  private var safe = true

  override def transform(e: Expr): Expr = e match {
    case LocalExpr(_, bs, _) =>
      bs.zipWithIndex.foreach { case (b, i) =>
        val key = scope.size+i
        //assert(!defsByBind.containsKey(b))
        if(!defsByBind.containsKey(b)) {
          val d = new Def(key)
          defsByBind.put(b, d)
        }
      }
      super.transform(e)
    case ObjBody.MemberList(_, bs, _, _) if bs != null =>
      bs.zipWithIndex.foreach { case (b, i) =>
        val key = scope.size+2+i
        //assert(!defsByBind.containsKey(b), s"unexpected duplicate Bind: $b")
        if(!defsByBind.containsKey(b)) {
          val d = new Def(key)
          defsByBind.put(b, d)
        }
      }
      super.transform(e)
    case ValidId(_, name, idx) =>
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
    case _: Comp | _: ObjBody.ObjComp => //TODO treat first generator as safe
      val prevSafe = safe
      safe = false
      val res = super.transform(e)
      safe = prevSafe
      res
    case _ => super.transform(e)
  }
}

class Inliner(rc: RefCounter) extends ScopedExprTransform {
  override def transform(e: Expr): Expr = e match {
    case e @ ValidId(_, name, idx) =>
      val sv = scope.get(name)
      if(sv != null && sv.bind != null && sv.bind.args == null) { //TODO handle functions
        val d = rc.defsByBind.get(sv.bind)
        if(d != null && d.all == 1 && d.safe == 1) {
          d.all = 0
          d.safe = 0
          println(s"---- inlining $name -> ${sv.bind.rhs}")
          sv.bind.rhs
        } else e
      } else e

    case _ => super.transform(e)
  }
}

class DefEliminator(rc: RefCounter) extends ExprTransform {
  private val eliminated = new java.util.BitSet
  private val transformedBinds = new java.util.IdentityHashMap[Bind, Bind]

  private def process(bs: Array[Bind]): (Array[Bind], mutable.ArrayBuffer[Int]) = {
    var res: mutable.ArrayBuilder.ofRef[Bind] = null
    var elim: mutable.ArrayBuffer[Int] = null
    var i = 0
    while(i < bs.length) {
      val b = bs(i)
      val d = rc.defsByBind.get(b)
      val eliminate = d.all match {
        case 0 => true
        //case 1 if d.safe == 1 => true
        case _ => false
      }
      if(eliminate) {
        if(res == null) {
          res = new mutable.ArrayBuilder.ofRef[Bind]
          if(i > 0) res.addAll(bs.iterator.take(i))
          elim = new mutable.ArrayBuffer
        }
        elim.addOne(d.idx)
      } else {
        if(res != null) res.addOne(b)
      }
      i += 1
    }
    if(res == null) (bs, null) else (res.result(), elim)
  }

  def translate(idx: Int): Int = {
    assert(!eliminated.get(idx))
    var res = idx
    var i = 0
    while(i < idx) {
      if(eliminated.get(i)) res -= 1
      i += 1
    }
    res
  }

  override protected[this] def transformBind(b: Bind): Bind = {
    transformedBinds.get(b) match {
      case null =>
        val b2 = super.transformBind(b)
        transformedBinds.put(b, b2)
        b2
      case b2 => b2
    }
  }

  def transform(e: Expr): Expr = e match {
    case LocalExpr(pos, bs, ret) =>
      val (bs2, elim) = process(bs)
      if(bs2 eq bs) rec(e)
      else {
        assert(bs2.length + elim.length == bs.length)
        elim.foreach(i => eliminated.set(i))
        val ret2 = transform(ret)
        val bs3 = transformBinds(bs2)
        elim.foreach(i => eliminated.clear(i))
        if(bs3.length == 0) ret2
        else LocalExpr(pos, bs3, ret2)
      }
    case ObjBody.MemberList(pos, bs, fs, as) if bs != null =>
      val (bs2, elim) = process(bs)
      if(bs2 eq bs) rec(e)
      else {
        assert(bs2.length + elim.length == bs.length, s"${bs2.length} + ${elim.length} should be ${bs.length}")
        elim.foreach(i => eliminated.set(i))
        val fs2 = transformFields(fs)
        val as2 = transformAsserts(as)
        val bs3 = transformBinds(bs2)
        elim.foreach(i => eliminated.clear(i))
        ObjBody.MemberList(pos, bs3, fs2, as2)
      }
    case e @ ValidId(_, _, nameIdx) =>
      val tr = translate(nameIdx)
      if(tr == nameIdx) e else e.copy(nameIdx = tr)
    case e @ SelectSuper(_, selfIdx, _) =>
      val tr = translate(selfIdx)
      if(tr == selfIdx) e else e.copy(selfIdx = tr)
    case e @ InSuper(_, value, selfIdx) =>
      val tr = translate(selfIdx)
      if(tr == selfIdx) e else e.copy(value=transform(value), selfIdx = tr)
    case e @ LookupSuper(_, selfIdx, index) =>
      val tr = translate(selfIdx)
      if(tr == selfIdx) e else e.copy(selfIdx = tr, index=transform(index))
    case _ => rec(e)
  }
}
