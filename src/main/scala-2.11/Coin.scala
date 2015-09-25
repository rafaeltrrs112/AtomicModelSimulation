/**
 * Sealed coin trait. Sealed use for compile time exhaustive check.
 */
//Vending machine specific code
/**
 * Some coin with a some currency value
 */
trait Coin extends SimulationToken {
  override val value : Int
}
/**
 * Nickel : five cents
 */
case class Nickel() extends Coin {
  override val value : Int = 5
}

/**
 * Dime : ten cents
 */
case class Dime() extends Coin {
  override val value : Int = 10
}

/**
 * Quarter : twenty five cents
 */
case class Quarter() extends Coin{
  override val value: Int = 25
}
/**
 *
 * @param entry
 *             The value of the cancel input into the vending machine
 */
case class Cancel(entry : Boolean) extends SimulationToken{
  override val value: Boolean = true
}
case class Change(change : Seq[Coin])
object Coin{
  val Nickel = "Nickel"
  val Dime = "Dime"
  val Quarter = "Quarter"
}