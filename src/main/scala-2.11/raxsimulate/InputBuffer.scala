package raxsimulate

import raxsimulate.io.{EmptyToken, Token}
import raxsimulate.model.Model

import scala.collection.mutable

/**
 */
class InputBuffer(modelName : String, initialOutput : Token) extends Model {
  override def name: String = modelName

  var inMemory : Option[Token] = Some(initialOutput)

  var _currentOutput : Option[Token] = None

  override def currentState  = mutable.Map[String, String](
    ("Next Output : ", currentOutput.toString),
    ("In memory:", inMemory.toString)
  )

  override def currentOutput : Option[Seq[Token]] = 
    if (_currentOutput.isDefined) Some(Seq(_currentOutput.get)) else None

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
  override def stateTransition(input: Seq[Token]): Unit = {
    input.foreach{
      case e : EmptyToken => emptyPushMemUp()
      case x : Token => pushMemUp(x)
    }
  }

  override def toString  = {
    currentState.toString()
  }

}
