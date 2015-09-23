/**
 * System Simulation Assignment 1
 * Vending Machine
 * */
//No new public methods should be created for mode AT ALL!!
//Only private ones, all models must be able to interface with
//the FrameWork handler.
object VendingMachine extends Model{
  override var currentState: State = _ // Default state

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  override def insert(input: Input*): Unit = ???

  /**
   *
   * @return
   * Pair containing the output if any, and a state that will
   * always exits.
   */
  override def outputAndView: (Option[Output], State) = ???

  /**
   * State transition functions alters the state member of the
   * model. A helper method will be used to set this function
   * to the the one returned when makeTransitionFunction is called.
   * @param input
   * Input into the model.
   */
  override def transitionState(input: Input*): Unit = ???




  /**
   * Some list of stateTransition functions that can be
   * retrieved from below.
   */
}
class VendingMachine()