package VendingMachine

/**
 * Sealed coin trait. Sealed use for compile time exhaustive check.
 */
trait Input[T] extends Output[T] {
  override val value: T
}
