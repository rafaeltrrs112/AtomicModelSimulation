package vendingmachine

import raxsimulate.Output

/**
 * Helper object for the Coin class
 */
object Coin {
  val Nickel = "Nickel"
  val Dime = "Dime"
  val Quarter = "Quarter"
}

/**
 * Some coin with a some currency value
 */
trait Coin extends Output[Int] {
  override val value: Int
}