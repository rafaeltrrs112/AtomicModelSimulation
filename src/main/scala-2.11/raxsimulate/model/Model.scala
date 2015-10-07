package raxsimulate.model

import raxsimulate.io.Token

/**
 * Model trait for the framework
 */
trait Model {

  def name : String

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
