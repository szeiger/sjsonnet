package sjsonnet

import java.util

import scala.jdk.CollectionConverters._


class MemberMap private (
  //private var keys: Array[String],
  private val map: java.util.LinkedHashMap[String, Val.Obj.Member],
  //private val map: java.util.HashMap[String,Val.Obj.Member],
  //private var len: Int
) {
  def this(initialSize: Int) = this(
    //new Array[String](initialSize),
    new java.util.LinkedHashMap[String, Val.Obj.Member](initialSize)
    //new java.util.HashMap[String,Val.Obj.Member](initialSize*3/2),
    //0
  )

  def put(k: String, v: Val.Obj.Member): Unit = {
    map.put(k, v)
    /*if(map.put(k, v) == null) {
      if(keys.length <= len) {
        keys = util.Arrays.copyOf(keys, keys.length*2)
      }
      keys(len) = k
      len += 1
    }*/
  }

  def forEach[U](f: (String, Val.Obj.Member) => U): Unit = {
    map.forEach((s, m) => f(s, m))
    /*var i = 0
    while(i < len) {
      val k = keys(i)
      f(k, map.get(k))
      i += 1
    }*/
  }

  @inline def get(k: String): Val.Obj.Member = map.get(k)

  def isStatic: Boolean =
    map.asScala.forall { case (k, m) => m.isInstanceOf[Val.Obj.ConstMember] && !m.add && m.visibility == Expr.Member.Visibility.Normal }

  def copy(): MemberMap =
    new MemberMap(map.clone().asInstanceOf[java.util.LinkedHashMap[String,sjsonnet.Val.Obj.Member]])
  //def copy(): MemberMap =
  //  new MemberMap(keys.clone(), map.clone().asInstanceOf[java.util.HashMap[String,sjsonnet.Val.Obj.Member]], len)
}

object MemberMap {
  @inline def apply(initialSize: Int): MemberMap = new MemberMap(initialSize)
  @inline def apply(): MemberMap = new MemberMap(16)
}