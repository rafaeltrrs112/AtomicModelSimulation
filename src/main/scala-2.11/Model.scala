/**
 * Model trait for the framework
 */
trait Model[A] {
  /**
   *
   * @param input
   *             Takes in input and update internal state independent
   *             variables. Calls transitionState() after input processing
   *             to alter state dependent model properties and and transition
   *             state.
   *
   */
  def stateTransition(input : Seq[SimulationToken])

  /**
   *
   * @return
   *        Pair containing the output if any, and a state that will
   *        always exits.
   */
  def outputAndView : (Option[Output])
}
