package raxsim.io

/**
 * All inputs can be outputted, but not all outputs can be inputted
 * into a model.
 */
trait Input[T] extends InputOutput[T] {
  override val value: T
}
trait InputOutput[T]{
  def value : T
}
trait Output[T] extends InputOutput[T]{
  def value : T
}
