package sjsonnet

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SymbolMap[T >: Null] private (keys: ArrayBuffer[String],
                                    values: ArrayBuffer[T],
                                    indices: mutable.HashMap[String, Int]) {

  def this(initialSize: Int) = this(
    new ArrayBuffer[String](initialSize),
    new ArrayBuffer[T](initialSize),
    new mutable.HashMap[String, Int](initialSize, mutable.HashMap.defaultLoadFactor)
  )

  def this() = this(mutable.HashMap.defaultInitialCapacity)

  def size: Int = keys.length

  def put(k: String, v: T): Unit = {
    val idx = indices.getOrElse(k, -1)
    if(idx == -1) {
      val i = keys.length
      keys.addOne(k)
      values.addOne(v)
      indices.put(k, i)
    } else {
      values(idx) = v
    }
  }

  def forEach[U](f: (String, T) => U): Unit = {
    var i = 0
    while(i < keys.length) {
      f(keys(i), values(i))
      i += 1
    }
  }

  def containsKey(k: String): Boolean = indices.contains(k)

  def get(k: String): T = {
    val idx = indices.getOrElse(k, -1)
    if(idx == -1) null else values(idx)
  }

  def isEmpty: Boolean = keys.isEmpty

  def keysToArray(): Array[String] = keys.toArray[String]

  def mapKeysToValues[U >: Null](f: String => U): SymbolMap[U] =
    new SymbolMap[U](keys, keys.map(f), indices)
}
