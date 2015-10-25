package Asims454.a2

import raxsim.io.Output

/**
 * Some coin with a some currency value
 */
sealed trait Coin extends Output[Int] {
  override val value: Int
}

/**
 * Helper object for the Coin class
 */
object Coin {
  val Nickel = "Nickel"
  val Dime = "Dime"
  val Quarter = "Quarter"
}

/**
 * Dime : ten cents
 */
case class Dime() extends Coin {
  override val value: Int = 10
}

/**
 * Nickel : five cents
 */
case class Nickel() extends Coin {
  override val value: Int = 5
}

/**
 * Quarter : twenty five cents
 */
case class Quarter() extends Coin {
  override val value: Int = 25
}

/**
 * A coin with no value.
 */
case class NoCoin() extends Coin {
  override val value = 0
}

