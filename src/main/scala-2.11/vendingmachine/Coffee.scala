package vendingmachine

import raxsimulate.Output

/**
 * Coffee output
 */
case class Coffee() extends Output[String] {
  override val value = "Starbucks (C)"
  override val toString = "Starbucks (C)"
}
