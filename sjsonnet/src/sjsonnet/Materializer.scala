package sjsonnet
import sjsonnet.Expr.{FieldName, Member, ObjBody}
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Lazy
import upickle.core.Visitor

import scala.collection.mutable

/**
  * Serializes the given [[Val]] out to the given [[upickle.core.Visitor]],
  * which can transform it into [[ujson.Value]]s or directly serialize it
  * to `String`s
  */
object Materializer {
  //private val dummyPos: Position = new Position(null, 0)

  def apply(v: Val, storePos: Position => Unit = _ => ())(implicit evaluator: EvalScope): ujson.Value = apply0(v, ujson.Value)
  def stringify(v: Val)(implicit evaluator: EvalScope): String = {
    apply0(v, new sjsonnet.Renderer()).toString
  }

  def apply0[T](v: Val, visitor: Visitor[T, T], storePos: Position => Unit = _ => ())
               (implicit evaluator: EvalScope): T = try {
    v match {
      case Val.True(pos) => storePos(pos); visitor.visitTrue(-1)
      case Val.False(pos) => storePos(pos); visitor.visitFalse(-1)
      case Val.Null(pos) => storePos(pos); visitor.visitNull(-1)
      case Val.Num(pos, n) => storePos(pos); visitor.visitFloat64(n, -1)
      case Val.Str(pos, s) => storePos(pos); visitor.visitString(s, -1)
      case xs: Val.Arr =>
        storePos(xs.pos);
        val arrVisitor = visitor.visitArray(xs.length, -1)
        for(x <- xs) {
          arrVisitor.visitValue(
            apply0(x, arrVisitor.subVisitor.asInstanceOf[Visitor[T, T]], storePos),
            -1
          )
        }
        arrVisitor.visitEnd(-1)

      case obj: Val.Obj =>
        storePos(obj.pos)
        obj.triggerAllAsserts(obj)

        val keysUnsorted = obj.visibleKeyNames
        val keys = if (!evaluator.preserveOrder) keysUnsorted.sorted else keysUnsorted
        val objVisitor = visitor.visitObject(keys.length , -1)

        for(k <- keys) {
          val value = obj.value(k, evaluator.emptyMaterializeFileScopePos)

          storePos(
            value match{
              case v: Val.Obj if v.hasKeys => value.pos
              case v: Val.Arr if v.length > 0 => value.pos
              case _ => null
            }
          )
          objVisitor.visitKeyValue(objVisitor.visitKey(-1).visitString(k, -1))



          objVisitor.visitValue(
            apply0(value, objVisitor.subVisitor.asInstanceOf[Visitor[T, T]], storePos),
            -1
          )
        }
        objVisitor.visitEnd(-1)

      case f: Val.Func =>
        apply0(
          f.apply(emptyStringArray, emptyLazyArray, evaluator.emptyMaterializeFileScopePos),
          visitor,
          storePos
        )
    }

  }catch {case e: StackOverflowError =>
    throw Error.Delegate("Stackoverflow while materializing, possibly due to recursive value")
  }

  def reverse(pos: Position, v: ujson.Value): Val = v match{
    case ujson.True => Val.True(pos)
    case ujson.False => Val.False(pos)
    case ujson.Null => Val.Null(pos)
    case ujson.Num(n) => Val.Num(pos, n)
    case ujson.Str(s) => Val.Str(pos, s)
    case ujson.Arr(xs) => new Val.Arr(pos, xs.map(x => (() => reverse(pos, x)): Val.Lazy).toArray[Val.Lazy])
    case ujson.Obj(xs) =>
      val builder = new java.util.LinkedHashMap[String,Val.Obj.Member](xs.size*3/2)
      for(x <- xs) {
        val v = new Val.Obj.Member(false, Visibility.Normal) {
          def invoke(self: Val.Obj, sup: Val.Obj, fs: FileScope, ev: EvalScope): Val = reverse(pos, x._2)
        }
        builder.put(x._1, v)
      }
      new Val.Obj(pos, builder, false, null, null)
  }

  def toExpr(v: ujson.Value)(implicit ev: EvalScope): Expr = v match{
    case ujson.True => Val.True(ev.emptyMaterializeFileScopePos)
    case ujson.False => Val.False(ev.emptyMaterializeFileScopePos)
    case ujson.Null => Val.Null(ev.emptyMaterializeFileScopePos)
    case ujson.Num(n) => Val.Num(ev.emptyMaterializeFileScopePos, n)
    case ujson.Str(s) => Val.Str(ev.emptyMaterializeFileScopePos, s)
    case ujson.Arr(xs) => Expr.Arr(ev.emptyMaterializeFileScopePos, xs.map(toExpr).toArray[Expr])
    case ujson.Obj(kvs) =>
      ObjBody.MemberList(
        ev.emptyMaterializeFileScopePos,
        null,
        null,
        null,
        for((k, v) <- kvs.toArray)
          yield Member.Field(ev.emptyMaterializeFileScopePos, FieldName.Fixed(k), false, null, Visibility.Normal, toExpr(v)),
        null
      )
  }

  val emptyStringArray = new Array[String](0)
  val emptyLazyArray = new Array[Lazy](0)

}
