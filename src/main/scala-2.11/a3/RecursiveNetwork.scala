package a3

import raxsim.io.{EmptyToken, Token}
import raxsim.model.{InputBuffer, Model}

import scala.collection.mutable

/**
 *
 * @param modelName
 * @param receiver
 * @param memory
 */
class RecursiveNetwork(modelName : String, receiver : Model, memory : InputBuffer) extends Model {
  var _currentOutput = IndexedSeq[Token]()

  /**
   *
   * @return
   */
  override val name: String = modelName

  /**
   *
   * @return
   */
  override def currentState: mutable.Map[String, String] = mutable.Map(("Current output: ", currentOutput.toString))

  /**
   *
   * @return
   */
  override def currentOutput: Option[IndexedSeq[Token]] = receiver.currentOutput

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   *
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = {
    receiver.stateTransition(
      IndexedSeq(input.head, input(1)) ++ memory.currentOutput.getOrElse(EmptyToken.emptyTokenIndexedSeq)
    )
  }

  /**
   *
   * @return
   */
  override def toString = {
    "recurse_net " + modelName + " output: " + currentOutput
  }
}
