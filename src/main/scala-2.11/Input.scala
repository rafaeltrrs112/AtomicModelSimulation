/**
 * All inputs are entered into a system for a reason. Therefore all inputs
 * must have at least one value. All inputs must be defined here so the compiler helps with
 * checking whether pattern matching is exhaustive.
 */
sealed trait Input {
  val value : AnyVal
}
/**
 *
 * @param input
 *              Sequence of one or inputs that. Must implement the Input trait.
 */
case class InputStream(input : Input*)

//Vending machine specific code
/**
 * Some coin with a some currency value
 */
trait Coin extends Input {
  override val value : Int
}
/**
 * Nickel : five cents
 */
case class Nickel() extends Coin {
    override val value = 5
}

/**
 * Dime : ten cents
 */
case class Dime() extends Coin {
  override val value = 10
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
case class Cancel(entry : Boolean) extends Input {
  override val value: AnyVal = entry
}