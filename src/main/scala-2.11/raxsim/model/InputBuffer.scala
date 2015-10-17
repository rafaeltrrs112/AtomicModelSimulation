package raxsim.model

import raxsim.io.{EmptyToken, Token}
import scala.collection.mutable

/**
 * Single input buffer.
 * Use a buffer with predefined bounds maybe...
 */
class InputBuffer(modelName : String, nextOutput : Token, initialOutput : Token) extends Model {
  override def name: String = modelName


  //Check if uninitialized in state transition function.
  var inMemory : Option[Token] = Some(nextOutput)

  var _currentOutput : Option[Token] = Some(initialOutput)

  override def currentState  = mutable.Map[String, String](
    ("Next Output : ", currentOutput.toString),
    ("In memory:", inMemory.toString)
  )

  override def currentOutput : Option[IndexedSeq[Token]] =
    if (_currentOutput.isDefined) Some(IndexedSeq(_currentOutput.get)) else None

  def pushMemUp(token : Token): Unit = {
    _currentOutput = inMemory
    inMemory = Some(token)
  }

  def emptyPushMemUp() = {
    //Push top in memory up
    _currentOutput = inMemory
    //Push 2nd in memory up and place new input into the back of the memory
    inMemory = None
  }

  /**
   *
   * @param input
   *    A single XorToken input to be stored in memory.
   *
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = {
    input.foreach{
      case e : EmptyToken => emptyPushMemUp()
      case x : Token => pushMemUp(x)
    }
  }

  override def toString  = {
    currentState.toString()
  }

}
