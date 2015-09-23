/**
 * Model trait for the framework
 */
abstract class Model {
  //Create a getter for the state
  abstract var currentState : State
  /**
   *
   * @param input
   *             Takes in input and update internal state independent
   *             variables. Calls transitionState() after input processing
   *             to alter state dependent model properties and and transition
   *             state.
   *
   */
  def insert(input : Input*)
  /**
   * State transition functions alters the state member of the
   * model. A helper method will be used to set this function
   * to the the one returned when makeTransitionFunction is called.
   * @param input
   *              Input into the model.
   */
  def transitionState(input : Input*) : Unit
  /**
   *
   * @return
   *        Pair containing the output if any, and a state that will
   *        always exits.
   */
  def outputAndView : (Option[Output] , State)
}
