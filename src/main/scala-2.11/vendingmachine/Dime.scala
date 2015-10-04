package vendingmachine

/**
 * Dime : ten cents
 */
case class Dime() extends Coin {
  override val value: Int = 10
}
