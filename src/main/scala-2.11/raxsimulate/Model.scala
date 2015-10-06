package raxsimulate

/**
 * Model trait for the framework
 */
trait Model {

  def currentOutput : Option[Seq[Token]]

  def currentState: scala.collection.mutable.Map[String, String]

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   *
   */
  def stateTransition(input: Seq[Token])
}
