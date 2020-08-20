package org.garagescience.vhi

// right. let's keep the design very simple

// all we require is that the type that's being passed in
// is Numeric (or maybe Integral), & that we have a strict
// limit on the types of numbers that we accept.

/* this style of thing:

 private def checkOverflow(res: Int, x: Byte, y: Byte, op: String): Unit = {
    if (res > Byte.MaxValue || res < Byte.MinValue) {
      throw new ArithmeticException(s"$x $op $y caused overflow.")
    }
  }

 */


import scala.util._
import scala.util.control.Exception._
import scala.collection.immutable.BitSet

sealed abstract class IntegralToBitset[T: Integral] {
  def getBitSet(t: T): Try[Either[String, BitSet]]
}

object ComparableOps {

  // usage:

  // println(compare[Int, Int](1, "<", 2, (a,b) => a-b))
  // val a: Byte = 1
  // val b: Short = 2

  // println(compare[Byte, Short](a, "<", b, (a,b) => a-b))

  def compare[T, U](a: T, op: String, b: U, comparator: (T, U) => Int): Boolean = {
    op match {
      case "==" => comparator(a, b) == 0
      case "<" => comparator(a, b) < 0
      case ">" => comparator(a, b) > 0
      case "<=" => comparator(a, b) <= 0
      case ">=" => comparator(a, b) >= 0
    }
  }
}

