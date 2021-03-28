package sjsonnet

import java.util

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

abstract class Eval {
  def force: Val
}

/**
 * [[Lazy]] models lazy evaluation within a Jsonnet program. Lazily
 * evaluated dictionary values, array contents, or function parameters
 * are all wrapped in [[Lazy]] and only truly evaluated on-demand
 */
abstract class Lazy extends Eval {
  private[this] var cached: Val = null
  def compute(): Val
  final def force: Val = {
    if(cached == null) cached = compute()
    cached
  }
}

/**
  * [[Val]]s represented Jsonnet values that are the result of evaluating
  * a Jsonnet program. The [[Val]] data structure is essentially a JSON tree,
  * except evaluation of object attributes and array contents are lazy, and
  * the tree can contain functions.
  */
sealed abstract class Val extends Eval {
  def prettyName: String
  def cast[T: ClassTag: PrettyNamed] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else throw new Error.Delegate(
      "Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName
    )
  def pos: Position
  final def force: Val = this
}
class PrettyNamed[T](val s: String)
object PrettyNamed{
  implicit def strName: PrettyNamed[Val.Str] = new PrettyNamed("string")
  implicit def numName: PrettyNamed[Val.Num] = new PrettyNamed("number")
  implicit def arrName: PrettyNamed[Val.Arr] = new PrettyNamed("array")
  implicit def objName: PrettyNamed[Val.Obj] = new PrettyNamed("object")
  implicit def funName: PrettyNamed[Val.Func] = new PrettyNamed("function")
}
object Val{
  abstract class Bool extends Val with Expr

  def bool(pos: Position, b: Boolean) = if (b) True(pos) else False(pos)

  case class True(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  case class False(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  case class Null(pos: Position) extends Val with Expr {
    def prettyName = "null"
  }
  case class Str(pos: Position, value: String) extends Val with Expr {
    def prettyName = "string"
  }
  case class Num(pos: Position, value: Double) extends Val with Expr {
    def prettyName = "number"
  }

  case class Arr(pos: Position, value: Array[_ <: Eval]) extends Val{
    def prettyName = "array"
  }
  class StaticArr(_pos: Position, _value: Array[_ <: Val]) extends Arr(_pos, _value) with Expr

  object Obj{

    case class Member(add: Boolean,
                      visibility: Visibility,
                      invoke: (Obj, Obj, FileScope, EvalScope) => Val,
                      cached: Boolean = true)


  }
  final class Obj(val pos: Position,
                  value0: mutable.Map[String, Obj.Member],
                  triggerAsserts: Val.Obj => Unit,
                  `super`: Obj) extends Val{

    def getSuper = `super`

    @tailrec def triggerAllAsserts(obj: Val.Obj): Unit = {
      if(triggerAsserts != null) triggerAsserts(obj)
      if(`super` != null) `super`.triggerAllAsserts(obj)
    }

    def addSuper(pos: Position, lhs: Val.Obj): Val.Obj = {
      `super` match{
        case null => new Val.Obj(pos, value0, null, lhs)
        case x => new Val.Obj(pos, value0, null, x.addSuper(pos, lhs))
      }
    }

    def prettyName = "object"

    private def gatherVisibleKeys(mapping: util.LinkedHashMap[String, java.lang.Boolean]): Unit = {
      if(`super` != null) `super`.gatherVisibleKeys(mapping)
      for((k, m) <- value0) {
        val vis = m.visibility
        if(!mapping.containsKey(k)) mapping.put(k, vis == Visibility.Hidden)
        else if(vis == Visibility.Hidden) mapping.put(k, true)
        else if(vis == Visibility.Unhide) mapping.put(k, false)
      }
    }

    lazy val visibleKeys = {
      val m = new util.LinkedHashMap[String, java.lang.Boolean]
      gatherVisibleKeys(m)
      m
    }

    def getVisibleKeyNamesArray = visibleKeys.keySet().toArray(new Array[String](visibleKeys.size()))

    def getVisibleKeysNonHiddenCount = {
      var c = 0
      visibleKeys.values().forEach(b => if(b == java.lang.Boolean.FALSE) c += 1)
      c
    }

    def getVisibleKeys: Array[(String, java.lang.Boolean)] = {
      val a = new Array[(String, java.lang.Boolean)](visibleKeys.size())
      var i = 0
      visibleKeys.forEach { (k, b) =>
        a(i) = (k, b)
        i += 1
      }
      a
    }

    def getVisibleKeysNonHidden: Array[String] = {
      val buf = mutable.ArrayBuilder.make[String]
      visibleKeys.forEach((k, b) => if(b == java.lang.Boolean.FALSE) buf += k)
      buf.result()
    }

    private[this] val valueCache = mutable.HashMap.empty[Any, Val]

    def value(k: String,
              pos: Position,
              self: Obj = this)
             (implicit evaluator: EvalScope): Val = {

      val cacheKey = if(self eq this) k else (k, self)

      valueCache.getOrElse(cacheKey, {
        valueRaw(k, self, pos, valueCache, cacheKey) match {
          case null => Error.fail("Field does not exist: " + k, pos)
          case x => x
        }
      })
    }

    private def renderString(v: Val)(implicit evaluator: EvalScope): String = {
      try evaluator.materialize(v).transform(new Renderer()).toString
      catch Error.tryCatchWrap(pos)
    }

    def mergeMember(l: Val,
                    r: Val,
                    pos: Position)
                   (implicit evaluator: EvalScope) = {
      val lStr = l.isInstanceOf[Val.Str]
      val rStr = r.isInstanceOf[Val.Str]
      if(lStr || rStr) {
        val ll = if(lStr) l.asInstanceOf[Val.Str].value else renderString(l)
        val rr = if(rStr) r.asInstanceOf[Val.Str].value else renderString(r)
        Val.Str(pos, ll ++ rr)
      } else if(l.isInstanceOf[Val.Num] && r.isInstanceOf[Val.Num]) {
        val ll = l.asInstanceOf[Val.Num].value
        val rr = r.asInstanceOf[Val.Num].value
        Val.Num(pos, ll + rr)
      } else if(l.isInstanceOf[Val.Arr] && r.isInstanceOf[Val.Arr]) {
        val ll = l.asInstanceOf[Val.Arr].value
        val rr = r.asInstanceOf[Val.Arr].value
        Val.Arr(pos, ll ++ rr)
      } else if(l.isInstanceOf[Val.Obj] && r.isInstanceOf[Val.Obj]) {
        val ll = l.asInstanceOf[Val.Obj]
        val rr = r.asInstanceOf[Val.Obj]
        rr.addSuper(pos, ll)
      } else throw new MatchError((l, r))
    }

    def valueRaw(k: String,
                 self: Obj,
                 pos: Position,
                 addTo: mutable.HashMap[Any, Val] = null,
                 addKey: Any = null)
                (implicit evaluator: EvalScope): Val = {
      val s = this.`super`
      this.value0.getOrElse(k, null) match{
        case null =>
          if(s == null) null else s.valueRaw(k, self, pos, addTo, addKey)
        case m =>
          val v = if(s != null && m.add) {
            val merged = s.valueRaw(k, self, pos, null, null) match{
              case null => m.invoke(self, s, pos.fileScope, evaluator)
              case supValue =>
                mergeMember(supValue, m.invoke(self, s, pos.fileScope, evaluator), pos)
            }
            merged
          } else m.invoke(self, s, pos.fileScope, evaluator)
          if(addTo != null && m.cached) addTo(addKey) = v
          v
      }
    }
  }

  case class Func(pos: Position,
                  defSiteValScope: ValScope,
                  params: Params,
                  evalRhs: (ValScope, String, EvalScope, FileScope, Position) => Val,
                  evalDefault: (Expr, ValScope, EvalScope) => Val = null) extends Val {

    def prettyName = "function"

    def apply(argNames: Array[String], argVals: Array[_ <: Eval],
              thisFile: String,
              outerPos: Position)
             (implicit evaluator: EvalScope) = {

      val argsSize = argVals.length
      val simple = argNames == null && params.indices.length == argsSize
      val passedArgsBindingsI = if(argNames != null) {
        val arrI: Array[Int] = new Array(argsSize)
        var i = 0
        try {
          while (i < argsSize) {
            val aname = argNames(i)
            arrI(i) = if(aname != null) params.argIndices.getOrElse(
              aname,
              Error.fail(s"Function has no parameter $aname", outerPos)
            ) else params.indices(i)
            i += 1
          }
        } catch { case e: IndexOutOfBoundsException =>
          Error.fail("Too many args, function has " + params.names.length + " parameter(s)", outerPos)
        }
        arrI
      } else if(params.indices.length < argsSize) {
        Error.fail(
          "Too many args, function has " + params.names.length + " parameter(s)",
          outerPos
        )
      } else params.indices // Don't cut down to size to avoid copying. The correct size is argVals.length!

      val defaultArgsBindingIndices = params.defaultsOnlyIndices
      val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }

      val newScope: ValScope = {
        if(simple) {
          defSiteValScope match {
            case null => ValScope.createSimple(passedArgsBindingsI, argVals)
            case s => s.extendSimple(passedArgsBindingsI, argVals)
          }
        } else {
          lazy val newScope: ValScope = {
            val defaultArgsBindings = new Array[Eval](params.defaultsOnly.length)
            var idx = 0
            while (idx < params.defaultsOnly.length) {
              val default = params.defaultsOnly(idx)
              defaultArgsBindings(idx) = (() => evalDefault(default, newScope, evaluator)): Lazy
              idx += 1
            }
            val newBindingsI = util.Arrays.copyOf(defaultArgsBindingIndices, defaultArgsBindings.length + argVals.length)
            val newBindingsV = util.Arrays.copyOf(defaultArgsBindings, newBindingsI.length)
            System.arraycopy(passedArgsBindingsI, 0, newBindingsI, defaultArgsBindings.length, argVals.length)
            System.arraycopy(argVals, 0, newBindingsV, defaultArgsBindings.length, argVals.length)
            defSiteValScope match {
              case null => ValScope.createSimple(newBindingsI, newBindingsV)
              case s => s.extendSimple(newBindingsI, newBindingsV)
            }
          }
          validateFunctionCall(passedArgsBindingsI, params, outerPos, funDefFileScope, argsSize)
          newScope
        }
      }

      evalRhs(newScope, thisFile, evaluator, funDefFileScope, outerPos)
    }

    def validateFunctionCall(passedArgsBindingsI: Array[Int],
                             params: Params,
                             outerPos: Position,
                             defSiteFileScope: FileScope,
                             argListSize: Int)
                            (implicit eval: EvalScope): Unit = {

      val seen = new util.BitSet(argListSize)
      var idx = 0
      while (idx < argListSize) {
        val i = passedArgsBindingsI(idx)
        seen.set(i)
        idx += 1
      }

      if(argListSize != params.names.length || argListSize != seen.cardinality()) {
        seen.clear()
        val repeats = new util.BitSet(argListSize)

        idx = 0
        while (idx < argListSize) {
          val i = passedArgsBindingsI(idx)
          if (!seen.get(i)) seen.set(i)
          else repeats.set(i)
          idx += 1
        }

        Error.failIfNonEmpty(
          repeats,
          outerPos,
          (plural, names) => s"binding parameter a second time: $names",
          defSiteFileScope
        )
        val b = params.noDefaultIndices.clone().asInstanceOf[util.BitSet]
        b.andNot(seen)
        Error.failIfNonEmpty(
          b,
          outerPos,
          (plural, names) => s"Function parameter$plural $names not bound in call",
          defSiteFileScope // pass the definition site for the correct error message/names to be resolved
        )
        seen.andNot(params.allIndices)
        Error.failIfNonEmpty(
          seen,
          outerPos,
          (plural, names) => s"Function has no parameter$plural $names",
          outerPos.fileScope
        )
      }
    }
  }
}

/**
  * [[EvalScope]] models the per-evaluator context that is propagated
  * throughout the Jsonnet evaluation.
  */
trait EvalScope extends EvalErrorScope{
  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val

  def materialize(v: Val): ujson.Value

  val emptyMaterializeFileScope = new FileScope(wd / "(materialize)", Map())

  val preserveOrder: Boolean = false
}

object ValScope{
  def empty(size: Int) = new ValScope(null, null, null, new Array(size))

  def createSimple(newBindingsI: Array[Int],
                   newBindingsV: Array[_ <: Eval]) = {
    val arr = new Array[Eval](newBindingsI.length)
    var i = 0
    while(i < newBindingsI.length) {
      arr(newBindingsI(i)) = newBindingsV(i)
      i += 1
    }
    new ValScope(null, null, null, arr)
  }
}

/**
  * [[ValScope]]s which model the lexical scopes within
  * a Jsonnet file that bind variable names to [[Val]]s, as well as other
  * contextual information like `self` `this` or `super`.
  *
  * Note that scopes are standalone, and nested scopes are done by copying
  * and updating the array of bindings rather than using a linked list. This
  * is because the bindings array is typically pretty small and the constant
  * factor overhead from a cleverer data structure dominates any algorithmic
  * improvements
  *
  * The bindings array is private and only copy-on-write, so for nested scopes
  * which do not change it (e.g. those just updating `dollar0` or `self0`) the
  * bindings array can be shared cheaply.
  */
class ValScope(val dollar0: Val.Obj,
               val self0: Val.Obj,
               val super0: Val.Obj,
               bindings0: Array[_ <: Eval]) {

  def bindings(k: Int): Eval = bindings0(k)

  def extend(newBindingsI: Array[Expr.Bind] = null,
             newBindingsF: Array[(Val.Obj, Val.Obj) => Lazy] = null,
             newDollar: Val.Obj = null,
             newSelf: Val.Obj = null,
             newSuper: Val.Obj = null) = {
    val dollar = if (newDollar != null) newDollar else dollar0
    val self = if (newSelf != null) newSelf else self0
    val sup = if (newSuper != null) newSuper else super0
    new ValScope(
      dollar,
      self,
      sup,
      if (newBindingsI == null || newBindingsI.length == 0) bindings0
      else{
        val b = util.Arrays.copyOf(bindings0, bindings0.length, classOf[Array[Eval]])
        var i = 0
        while(i < newBindingsI.length) {
          b(newBindingsI(i).name) = newBindingsF(i).apply(self, sup)
          i += 1
        }
        b
      }
    )
  }

  def extendSimple(newBindingsI: Array[Int],
                   newBindingsV: Array[_ <: Eval]) = {
    if(newBindingsI == null || newBindingsI.length == 0) this
    else {
      val b = util.Arrays.copyOf(bindings0, bindings0.length, classOf[Array[Eval]])
      var i = 0
      while(i < newBindingsI.length) {
        b(newBindingsI(i)) = newBindingsV(i)
        i += 1
      }
      new ValScope(dollar0, self0, super0, b)
    }
  }

  def extendSimple(newBindingI: Int,
                   newBindingV: Eval) = {
    val b = util.Arrays.copyOf(bindings0, bindings0.length, classOf[Array[Eval]])
    b(newBindingI) = newBindingV
    new ValScope(dollar0, self0, super0, b)
  }
}
