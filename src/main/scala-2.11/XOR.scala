import VendingMachine.{Model, Token}

import scala.collection.mutable

/**
 * Assignment 3 XOR model.
 */
class XOR extends Model {
  override var _currentOutput: Option[Seq[Token]] = _

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   *
   */
  override def stateTransition(input: Seq[Token]): Unit = ???

  override var _currentState: mutable.Map[String, String] = _
}
