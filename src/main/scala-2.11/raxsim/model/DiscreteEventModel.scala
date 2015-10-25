package raxsim.model
import raxsim.io.{InputOutput, Input, Output, Token}

/**
 * A trait describing a Discrete event model. This trait is accessed by
 * a Discrete event simulation executor (not yet implemented).
 *
 * @tparam I
 *           The input type.
 * @tparam O
 *           The output type.
 * @tparam S
 *           The state type. This is also the output type of DeltaINT
 * @tparam A
 *           The type of @timeAdvance's argument
 * @tparam R
 *           The output type of deltaExt, and the input type of deltaINT

 */
trait DiscreteEventModel[-I <: DiscreteTimeInput[InputOutput[Int]], +O <: InputOutput[Int], S <:State, A <: Token, R <: State]{
  /**
   * All models must have a name to allow for networking in the future.
   */
  val name : String
  
  //lastEventTime is changed as input is received and holds the last RWT of Input
  /**
   * The RWT the last event occurred in the system.
   */
  var lastEventTime : Int

  /**
   * Trigger time function that may be set to some dynamic time or
   * made dynamic according to other dependencies in the system.
   * @return
   *         The least amount of time until the next event is triggered.
   */
  var triggerTime : Int

  /**
   * The current state of the model.
   * @return
   *         The state.
   */
  def state: S

  /**
   * Creates output according to the current state.
   *
   * @tparam C
   *           The type of output.
   * @return
   *         An @Option of @Output
   */
  def lambda[C >: O](state : S) : C

  /**
   * The external state transition function called when input is received from the outside.
   * @param state
   *              The current state of the model.
   * @param input
   *           Some input that @implemented @DiscreteTimeInput
   * @tparam C
   *           The type of input.
   */
  def deltaExternal[C <: I](state : S, input: C, elapsedTime : Int) : R //Changes the state of the model

  /**
   * Receive input from outside the model.
   * @param input
   *              An input into the model.
   * @tparam C
   *           The type of input.
   */
  def receive[C <: I](input : C, realWorldTime : Int) : S

  /**
   * The internal state transition function. Whose input is the curried output of
   * deltaExt. Is called when the internal event timer goes off.
   *
   * @param input
   *              Some input returned by @deltaExt.
   * @return
   *         The new @State of the 
   */
  def deltaInternal(input: R) : S

  /**
   * Called when the timer has gone off and input is being entered.
   *
   * @param input
   *           Some input that @implemented @DiscreteTimeInput
   * @param elapsedTime
   *                    The elapsed time computed from the clock time.
   * @param lastEventTime
   *                    The last time a system event
   * @tparam C
   *           The type of input.
   */
  def deltaConfluent[C <: I](input : C, elapsedTime : Int, lastEventTime : Int): S

  /**
   * The amount of time to wait before deltaInternal is called.
   * No input for timeAdvance that is determined by the abstract time advance
   * clause method.
   * @return
   *         The maximum amount of time to wait until triggering
   */
  def timeAdvance : Option[Int] = {
    if (advanceClause()) Some(triggerTime) else DiscreteEventModel.INF
  }
  //Time advance has a default implementation.

  /**
   * Runs some check to determine if the time advance function should trigger. If so
   * the trigger may be triggered in this method before as it will be return as the
   * time advance value here. If not then and INF time will be released from timeAdvance.
   * @return
   *         True if the advance clause has been met. False if not
   */
  def advanceClause() : Boolean
}

object DiscreteEventModel{
  val INF = None
}