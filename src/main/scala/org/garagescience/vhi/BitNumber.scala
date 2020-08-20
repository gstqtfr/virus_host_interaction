package org.garagescience.vhi

object BitNumber {

  abstract class Bit
  case object _0 extends Bit
  case object _1 extends Bit

  object Bit {
    val fromInt: Int => Bit = Array(_0, _1)
    val toInt: Bit => Int = Map(_0 -> 0, _1 -> 1)
  }

}