package raxsim.io

/**
 * All inputs can be outputted, but not all outputs can be inputted
 * into a model.
 */
sealed trait Input[T] extends InputOutput[T] {
  override val value: T
}
trait InputOutput[T]{
  def value : T
}
trait Output[T] extends InputOutput[T]{
  def value : T
}
/**
 *
 * @param entry
 * The value of the cancel input into the vending machine
 */
case class Cancel(entry: Boolean) extends Input[Boolean] {
  override val value: Boolean = entry
}
