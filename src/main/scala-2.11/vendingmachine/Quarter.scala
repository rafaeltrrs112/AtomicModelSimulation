package vendingmachine

/**
 * Quarter : twenty five cents
 */
case class Quarter() extends Coin {
  override val value: Int = 25
}
