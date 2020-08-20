package org.garagescience.vhi

import scala.util.Random
import scala.collection.immutable.BitSet
import scala.collection.{ AbstractIterator, SpecificIterableFactory,
  StrictOptimizedSeqOps, View, mutable }
import scala.collection.immutable.{ IndexedSeq, IndexedSeqOps }

/*
  basing this around:

  https://docs.scala-lang.org/overviews/core/custom-collections.html#first-version-of-rna-strands-class

    ... minus the RNA stuff, unfortunately ...

 */

// do we need this?
//import BitSetOps._

import scala.collection._

object FixedSizeSequence {

  import scala.collection._

  final class FixedSizeSeq[A] private (val capacity: Int, val length: Int, offset: Int, elems: Array[Any])
    extends immutable.Iterable[A]
      with IterableOps[A, FixedSizeSeq, FixedSizeSeq[A]]
      with IterableFactoryDefaults[A, FixedSizeSeq]
      with StrictOptimizedIterableOps[A, FixedSizeSeq, FixedSizeSeq[A]] { self =>

    def this(capacity: Int) =
      this(capacity, length = 0, offset = 0, elems = Array.ofDim(capacity))

    def appended[B >: A](elem: B): FixedSizeSeq[B] = {
      val newElems = Array.ofDim[Any](capacity)
      Array.copy(elems, 0, newElems, 0, capacity)
      val (newOffset, newLength) =
        if (length == capacity) {
          newElems(offset) = elem
          ((offset + 1) % capacity, length)
        } else {
          newElems(length) = elem
          (offset, length + 1)
        }
      new FixedSizeSeq[B](capacity, newLength, newOffset, newElems)
    }

    @`inline` def :+ [B >: A](elem: B): FixedSizeSeq[B] = appended(elem)

    def apply(i: Int): A = elems((i + offset) % capacity).asInstanceOf[A]

    def iterator: Iterator[A] = view.iterator

    override def view: IndexedSeqView[A] = new IndexedSeqView[A] {
      def length: Int = self.length
      def apply(i: Int): A = self(i)
    }

    override def knownSize: Int = length

    override def className = "FixedSizeSeq"

    override val iterableFactory: IterableFactory[FixedSizeSeq] = new FixedSizeSeqFactory(capacity)

  }

  class FixedSizeSeqFactory(capacity: Int) extends IterableFactory[FixedSizeSeq] {

    def from[A](source: IterableOnce[A]): FixedSizeSeq[A] =
      source match {
        case fixedSizeSeq: FixedSizeSeq[A] if fixedSizeSeq.capacity == capacity => fixedSizeSeq
        case _ => (newBuilder[A] ++= source).result()
      }

    def empty[A]: FixedSizeSeq[A] = new FixedSizeSeq[A](capacity)

    def newBuilder[A]: mutable.Builder[A, FixedSizeSeq[A]] =
      new mutable.ImmutableBuilder[A, FixedSizeSeq[A]](empty) {
        def addOne(elem: A): this.type = { elems = elems :+ elem; this }
      }

  }

}
