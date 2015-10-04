package VendingMachine

/**
 * Created by rtorres12 on 10/4/15.
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