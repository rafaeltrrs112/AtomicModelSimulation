package a3

import raxsimulate.io.{EmptyToken, Token}
import raxsimulate.model.Model
import raxsimulate.Simulation

import scala.collection.mutable

/**
 * Memory model for use in Homework 3 assignment.
 *
 */
//TODO Have this model now extends the memory model class
class XorMemory(modelName : String) extends Model {
  override def name: String = modelName
  
  var inMemory : (Option[XORToken], Option[XORToken]) = (Some(XORToken(true)), Some(XORToken(false)))
  var currentTop : Option[XORToken] = Some(XORToken(false))

  override def currentState  = mutable.Map[String, String](
    ("Next Output : ", currentOutput.toString),
    ("In memory:", inMemory.toString())
  )

  override def currentOutput : Option[Seq[Token]] = Some(Seq[XORToken](
    currentTop.getOrElse(XORToken(false)))
  )

  def pushMemUp(xORToken: XORToken): Unit = {
    //Push top in memory up
    currentTop = inMemory._1
    //Push 2nd in memory up and place new input into the back of the memory
    inMemory = (inMemory _2, Some(xORToken))
  }

  def emptyPushMemUp() = {
    //Push top in memory up
    currentTop = inMemory._1
    //Push 2nd in memory up and place new input into the back of the memory
    inMemory = (inMemory _2, None)
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
      case x : XORToken => pushMemUp(x)
    }
  }

  override def toString  = {
    currentState.toString()
  }

}
object XorList {

  /**
   * Apply method for creating a list of XOR tokens
   * @param xorTokens
   *                   A sequence of one or more booleans.
   * @return
   *          A sequence of XOR tokens whose order of boolean values is in the same order as those
   *          inserted as parameters.
   */
  def apply(xorTokens : Boolean*) : Seq[Seq[XORToken]] = {
    val xorTokenList = for(value <- xorTokens) yield Seq(XORToken(value))
    xorTokenList
  }

}
object XorMemoryTest extends App{
  val mem = new XorMemory("XorModel")
  val simulation = new Simulation(mem, XorList(true, false, true, true, false, false, true, true))
  simulation.runSimulation()
}