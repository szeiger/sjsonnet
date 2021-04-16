package sjsonnet

import Expr._

import scala.collection.mutable

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
        case m @ ObjBody.MemberList(pos, binds, fields, asserts) =>
          if(binds == null && asserts == null && fields.forall(_.isStatic)) Val.staticObject(pos, fields)
          else m
        case other => other
      }

//    case Import(pos, value) if resolver != null =>
//      resolver.resolveAndRead(pos.fileScope.currentFile.parent(), value) match {
//        case Some((p, str)) =>
//          resolver.parse(p, str) match {
//            case Right((doc, newFileScope)) =>
//              println(s"Resolved $pos, $p")
//              transform(ResolvedImport(pos, p, doc, newFileScope))
//            case Left(msg) => e
//          }
//        case None => e
//      }

    case e => super.transform(e)
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

class GlobalOptimizer(resolver: CachedResolver) {
  class Doc(val expr: Expr, val fs: FileScope) {
    val references = new mutable.HashSet[Position]
  }

  var allDocs = new mutable.HashMap[Path, Doc]

  def optimize(root: Expr): Expr = {
    (new Analyzer).transform(root)
    root
  }

  class Analyzer extends ExprTransform {
    def transform(expr: Expr): Expr = expr match {
      case e: LocalExpr => rec(e) // This is actually lazy but it's our best chance of finding imports
      case e: Lookup => rec(e)
      case e: Select => rec(e)
      case Apply(pos, value, argNames, argExprs) =>
        rec(value)
        expr
      case Import(pos, value) =>
        resolver.resolveAndRead(pos.fileScope.currentFile.parent(), value) match {
          case Some((p, str)) =>
            allDocs.get(p) match {
              case Some(d) =>
                d.references.add(pos)
              case None =>
                resolver.parse(p, str) match {
                  case Right((doc, newFileScope)) =>
                    val d = new Doc(doc, newFileScope)
                    //println(s"Importing: $p")
                    d.references.add(pos)
                    allDocs.put(p, d)
                    transform(doc)
                  case _ =>
                }
            }
          case _ =>
        }
        expr

      case e => e
    }
  }
}
