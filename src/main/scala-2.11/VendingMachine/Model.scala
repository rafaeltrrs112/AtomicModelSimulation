package VendingMachine

/**
 * Model trait for the framework
 */
trait Model {

  var _currentOutput: Option[Seq[Token]]

  def currentOutput = _currentOutput

  var _currentState: scala.collection.mutable.Map[String, String]

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
