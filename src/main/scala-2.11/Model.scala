/**
 * Model trait for the framework
 */
trait Model {
  //Create a getter for the state
  var _currentState : State
  /**
   *
   * @param input
   *             Takes in input and update internal state independent
   *             variables. Calls transitionState() after input processing
   *             to alter state dependent model properties and and transition
   *             state.
   *
   */
  def stateTransition(input : SimulationToken*)

  /**
   *
   * @return
   *        Pair containing the output if any, and a state that will
   *        always exits.
   */
  def outputAndView : (Option[Output] , State)
}
