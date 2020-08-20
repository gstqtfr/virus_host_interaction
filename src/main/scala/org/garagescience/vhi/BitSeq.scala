package org.garagescience.vhi

import scala.collection.{ AbstractIterator, SpecificIterableFactory, StrictOptimizedSeqOps, View, mutable }
import scala.collection.immutable.{ IndexedSeq, IndexedSeqOps }
import BitNumber._

final class BitSeq private (
                          val groups: Array[Int],
                          val length: Int
                        ) extends IndexedSeq[Bit]
  with IndexedSeqOps[Bit, IndexedSeq, BitSeq]
  with StrictOptimizedSeqOps[Bit, IndexedSeq, BitSeq] { bits =>

  import BitSeq._

  // Mandatory implementation of `apply` in `IndexedSeqOps`
  def apply(idx: Int): Bit = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    Bit.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }

  // Mandatory overrides of `fromSpecific`, `newSpecificBuilder`,
  // and `empty`, from `IterableOps`
  override protected def fromSpecific(coll: IterableOnce[Bit]): BitSeq =
    BitSeq.fromSpecific(coll)
  override protected def newSpecificBuilder: mutable.Builder[Bit, BitSeq] =
    BitSeq.newBuilder
  override def empty: BitSeq = BitSeq.empty

  // Overloading of `appended`, `prepended`, `appendedAll`, `prependedAll`,
  // `map`, `flatMap` and `concat` to return an `BitSeq` when possible
  def concat(suffix: IterableOnce[Bit]): BitSeq =
    strictOptimizedConcat(suffix, newSpecificBuilder)
  @inline final def ++ (suffix: IterableOnce[Bit]): BitSeq = concat(suffix)
  def appended(base: Bit): BitSeq =
    (newSpecificBuilder ++= this += base).result()
  def appendedAll(suffix: Iterable[Bit]): BitSeq =
    strictOptimizedConcat(suffix, newSpecificBuilder)
  def prepended(base: Bit): BitSeq =
    (newSpecificBuilder += base ++= this).result()
  def prependedAll(prefix: Iterable[Bit]): BitSeq =
    (newSpecificBuilder ++= prefix ++= this).result()
  def map(f: Bit => Bit): BitSeq =
    strictOptimizedMap(newSpecificBuilder, f)
  def flatMap(f: Bit => IterableOnce[Bit]): BitSeq =
    strictOptimizedFlatMap(newSpecificBuilder, f)

  // Optional re-implementation of iterator,
  // to make it more efficient.
  override def iterator: Iterator[Bit] = new AbstractIterator[Bit] {
    private var i = 0
    private var b = 0
    def hasNext: Boolean = i < bits.length
    def next(): Bit = {
      b = if (i % N == 0) groups(i / N) else b >>> S
      i += 1
      Bit.fromInt(b & M)
    }
  }

  override def className = "BitSeq"
}

object BitSeq extends SpecificIterableFactory[Bit, BitSeq] {

  private val S = 4            // number of bits in group
  private val M = (1 << S) - 1 // bitmask to isolate a group
  private val N = 32 / S       // number of groups in an Int

  def fromSeq(buf: collection.Seq[Bit]): BitSeq = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Bit.toInt(buf(i)) << (i % N * S)
    new BitSeq(groups, buf.length)
  }

  // Mandatory factory methods: `empty`, `newBuilder`
  // and `fromSpecific`
  def empty: BitSeq = fromSeq(Seq.empty)

  def newBuilder: mutable.Builder[Bit, BitSeq] =
    mutable.ArrayBuffer.newBuilder[Bit].mapResult(fromSeq)

  def fromSpecific(it: IterableOnce[Bit]): BitSeq = it match {
    case seq: collection.Seq[Bit] => fromSeq(seq)
    case _ => fromSeq(mutable.ArrayBuffer.from(it))
  }
}