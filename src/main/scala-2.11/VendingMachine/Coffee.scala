package VendingMachine

/**
 * Created by rtorres12 on 10/4/15.
 */
case class Coffee() extends Output[String] {
  override val value = "Starbucks (C)"
  override val toString = "Starbucks (C)"
}
