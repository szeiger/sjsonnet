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
class ValScope(val dollar0: Val.Obj,
               val self0: Val.Obj,
               val super0: Val.Obj,
               bindings0: Array[Val.Lazy]) {

  @inline def bindings(k: Int): Val.Lazy = bindings0(k)

  def length: Int = bindings0.length

  def extend(newBindingsF: Array[(Val.Obj, Val.Obj) => Val.Lazy] = null,
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
      if (newBindingsF == null || newBindingsF.length == 0) bindings0
      else {
        val b = Arrays.copyOf(bindings0, bindings0.length + newBindingsF.length)
        var i = 0
        var j = bindings0.length
        while(i < newBindingsF.length) {
          b(j) = newBindingsF(i).apply(self, sup)
          i += 1
          j += 1
        }
        b
      }
    )
  }

  def extendSimple(newBindingsV: Array[Val.Lazy]) = {
    if(newBindingsV == null || newBindingsV.length == 0) this
    else {
      val b = Arrays.copyOf(bindings0, bindings0.length + newBindingsV.length)
      System.arraycopy(newBindingsV, 0, b, bindings0.length, newBindingsV.length)
      new ValScope(dollar0, self0, super0, b)
    }
  }

  def extendSimple(l1: Val.Lazy) = {
    val b = Arrays.copyOf(bindings0, bindings0.length+1)
    b(bindings0.length) = l1
    new ValScope(dollar0, self0, super0, b)
  }

  def extendSimple(l1: Val.Lazy, l2: Val.Lazy) = {
    val b = Arrays.copyOf(bindings0, bindings0.length+2)
    b(bindings0.length) = l1
    b(bindings0.length+1) = l2
    new ValScope(dollar0, self0, super0, b)
  }
}

object ValScope{
  private[this] val emptyArr = new Array[Val.Lazy](0)
  def empty = new ValScope(null, null, null, emptyArr)

  def createSimple(newBindingV: Val.Lazy) = {
    val arr = new Array[Val.Lazy](1)
    arr(0) = newBindingV
    new ValScope(null, null, null, arr)
  }

  def createSimple(newBindingsV: Array[Val.Lazy]) =
    new ValScope(null, null, null, newBindingsV)

  final val INVALID_IDX = -1
}
