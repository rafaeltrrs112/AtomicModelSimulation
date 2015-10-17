package raxsim.model

import raxsim.io.{EmptyToken, Token}

/**
 * Model trait for the framework
 */
trait Model {

  def name : String

  def currentOutput : Option[IndexedSeq[Token]]

  def currentState: scala.collection.mutable.Map[String, String]

  /**
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  def stateTransition(input: IndexedSeq[Token])
}
