package sjsonnet

import java.util.Arrays

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
object ValScope{
  type ValScope = Array[Lazy]

  private final val DOLLAR = 0
  private final val SELF = 1
  private final val SUPER = 2

  private[this] val emptyArr = new Array[Lazy](3)
  def empty: ValScope = emptyArr

  def createSimple(a: Array[_ <: Lazy]) = {
    val b = new Array[Lazy](a.length+3)
    System.arraycopy(a, 0, b, 3, a.length)
    b
  }

  def createSimple(l1: Lazy) = {
    val arr = new Array[Lazy](4)
    arr(3) = l1
    arr
  }

  def createSimple(l1: Lazy, l2: Lazy) = {
    val arr = new Array[Lazy](5)
    arr(3) = l1
    arr(4) = l2
    arr
  }

  def createSimple(l1: Lazy, l2: Lazy, l3: Lazy) = {
    val arr = new Array[Lazy](6)
    arr(3) = l1
    arr(4) = l2
    arr(5) = l3
    arr
  }

  def createSimple(len: Int) = new Array[Lazy](len+3)

  def extend(vs:ValScope,
             newBindingsF: Array[(Val.Obj, Val.Obj) => Lazy] = null,
             newDollar: Val.Obj = null,
             newSelf: Val.Obj = null,
             newSuper: Val.Obj = null) = {
    val num = if(newBindingsF != null) newBindingsF.length else 0
    val b = Arrays.copyOf(vs, vs.length + num)
    if(newDollar != null) b(ValScope.DOLLAR) = newDollar
    if(newSelf != null) b(ValScope.SELF) = newSelf
    if(newSuper != null) b(ValScope.SUPER) = newSuper
    if(num > 0) {
      val self = b(ValScope.SELF).asInstanceOf[Val.Obj]
      val sup = b(ValScope.SUPER).asInstanceOf[Val.Obj]
      var i = 0
      var j = vs.length
      while(i < num) {
        b(j) = newBindingsF(i).apply(self, sup)
        i += 1
        j += 1
      }
    }
    b
  }

  def extendBy(vs: ValScope, num: Int, newDollar: Val.Obj, newSelf: Val.Obj, newSuper: Val.Obj) = {
    val b = Arrays.copyOf(vs, vs.length + num)
    if(newDollar != null) b(ValScope.DOLLAR) = newDollar
    if(newSelf != null) b(ValScope.SELF) = newSelf
    if(newSuper != null) b(ValScope.SUPER) = newSuper
    b
  }

  def extendSimple(vs: ValScope, newBindingsV: Array[_ <: Lazy]) = {
    if(vs == null) createSimple(newBindingsV)
    else if(newBindingsV == null || newBindingsV.length == 0) vs
    else {
      val b = Arrays.copyOf(vs, vs.length + newBindingsV.length)
      System.arraycopy(newBindingsV, 0, b, vs.length, newBindingsV.length)
      b
    }
  }

  def extendBy(vs: ValScope, num: Int) =
    if(vs == null) createSimple(num)
    else if(num == 0) vs
    else Arrays.copyOf(vs, vs.length + num)

  def extendSimple(vs: ValScope, l1: Lazy) = {
    if(vs == null) createSimple(l1)
    else {
      val b = Arrays.copyOf(vs, vs.length+1)
      b(vs.length) = l1
      b
    }
  }

  def extendSimple(vs: ValScope, l1: Lazy, l2: Lazy) = {
    if(vs == null) createSimple(l1, l2)
    else {
      val b = Arrays.copyOf(vs, vs.length+2)
      b(vs.length) = l1
      b(vs.length+1) = l2
      b
    }
  }

  def extendSimple(vs: ValScope, l1: Lazy, l2: Lazy, l3: Lazy) = {
    if(vs == null) createSimple(l1, l2, l3)
    else {
      val b = Arrays.copyOf(vs, vs.length+3)
      b(vs.length) = l1
      b(vs.length+1) = l2
      b(vs.length+2) = l3
      b
    }
  }

  @inline def getSuper(vs: ValScope) = vs(ValScope.SUPER).asInstanceOf[Val.Obj]
  @inline def getSelf(vs: ValScope) = vs(ValScope.SELF).asInstanceOf[Val.Obj]
  @inline def getDollar(vs: ValScope) = vs(ValScope.DOLLAR).asInstanceOf[Val.Obj]
}
