package vendingmachine

import raxsimulate.Output

/**
 * Error output
 */
case class Error() extends Output[String] {

  override val value = "Cannot output change, enter more coins or please\n" +
    "call Sim Soda Company at 1-800-123-4567"

  override val toString = "Cannot output change, enter more coins or please\n" +
    "call Sim Soda Company at 1-800-123-4567"
}
