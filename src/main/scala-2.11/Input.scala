/**
 * All inputs are entered into a system for a reason. Therefore all inputs
 * must have at least one value. All inputs must be defined here so the compiler helps with
 * checking whether pattern matching is exhaustive.
 */
trait SimulationToken{
  val value : AnyVal
}
case class Coffee() extends SimulationToken{
  override val value = ()
  override val toString = "Starbucks (C)"
}