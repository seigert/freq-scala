package freq

import java.nio.charset.StandardCharsets
import java.util

import it.unimi.dsi.fastutil.objects.ObjectArrays

import scala.annotation.tailrec

final class FrequencyDict(initial: Int) {
  import FrequencyDict.{ LoadFactor, Value }

  private var capacity = initial
  private var length   = 0
  private var mask     = capacity - 1
  private var max      = (LoadFactor * capacity.toFloat).toInt

  private var hashes = new Array[Int](capacity)
  private var values = new Array[Value](capacity)

  def size: Int = length

  def drain: Iterator[(Int, String)] = {
    val data = values

    hashes = new Array[Int](initial)
    values = new Array[Value](initial)

    capacity = initial
    length = 0
    mask = capacity - 1
    max = (LoadFactor * capacity.toFloat).toInt

    // scalafix:off DisableSyntax.null; keeping buckets sparse for locality
    ObjectArrays.unstableSort(data, { (l: Value, r: Value) =>
      if ((l ne null) && (r ne null)) l.compareTo(r)
      else if (l ne null) -1
      else if (r ne null) 1
      else 0
    })
    //scalafix:on

    data.iterator
      .takeWhile(_ ne null) // scalafix:ok DisableSyntax.null
      .map(v => (v.value, v.key))
  }

  def register(hash: Int, key: Array[Byte]): this.type = {
    val hsh = if (hash == 0) Fnv1.H else hash
    @tailrec def loop(idx: Int): Unit = {
      val idxHash = hashes(idx)
      if (idxHash == 0) {
        hashes(idx) = hsh
        values(idx) = Value(key)
        length += 1

        if (length > max) ensureCapacity()
      } else if (idxHash != hsh || !values(idx).update(key)) {
        loop((idx + 1) & mask)
      }
    }

    loop(hsh & mask)
    this
  }

  private def ensureCapacity(): Unit = {
    while (length > max) {
      capacity *= 2
      mask = capacity - 1
      max = (LoadFactor * capacity.toFloat).toInt
    }

    val newHashes = new Array[Int](capacity)
    val newValues = new Array[Value](capacity)

    hashes.iterator.zipWithIndex.filter(_._1 != 0).foreach {
      case (hash, i) =>
        var idx = hash & mask
        while (newHashes(idx) != 0) idx = (idx + 1) & mask

        newHashes(idx) = hash
        newValues(idx) = values(i)
    }

    hashes = newHashes
    values = newValues
  }

}

object FrequencyDict {

  final class Value(private val bytes: Array[Byte]) extends Comparable[Value] {
    private var counter: Int = 1

    lazy val key: String = new String(bytes, StandardCharsets.UTF_8)
    def value: Int       = counter

    def update(arr: Array[Byte]): Boolean = {
      val same = util.Arrays.equals(bytes, arr)
      if (same) counter += 1
      same
    }

    def compareTo(that: Value): Int = {
      var r = that.counter - this.counter
      var i = 0
      while (r == 0 && i < this.bytes.length && i < that.bytes.length) {
        r = this.bytes(i) - that.bytes(i)
        i += 1
      }

      if (r == 0 && i < that.bytes.length) -1
      else if (r == 0 && i < this.bytes.length) 1
      else r
    }
  }

  object Value {
    def apply(bytes: Array[Byte]): Value = {
      new Value(bytes)
    }
  }

  val InitialCapacity = 128
  val LoadFactor      = 0.9f

  def apply(): FrequencyDict = new FrequencyDict(InitialCapacity)

}
