package vendingmachine

import raxsimulate.Input

/**
 *
 * @param entry
 * The value of the cancel input into the vending machine
 */
case class Cancel(entry: Boolean) extends Input[Boolean] {
  override val value: Boolean = entry
}
