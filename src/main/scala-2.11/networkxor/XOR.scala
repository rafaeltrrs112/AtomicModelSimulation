package networkxor

import raxsimulate.{Simulation, Model, Token}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class XORToken(value : Boolean) extends Token
/**
 * Assignment 3 network xor.XOR model.
 */
class XORModel extends Model {

  var _currentOutput: Option[Seq[Token]] =
    Some(ArrayBuffer[XORToken](XORToken(true)))

  /**
   *
   * @param input
   * List of XOR tokens to process. True only if odd number of inputs are true
   *
   */
  override def stateTransition(input: Seq[Token]): Unit = {
    val result : Seq[XORToken] = {
      input.map(_.asInstanceOf[XORToken])
    }
    _currentOutput = Some(Seq[XORToken](result.foldLeft(XORToken(false))(XOR)))
    _currentState("Output: ") = _currentOutput.toString
  }

  private def XOR(xOne : XORToken, xTwo : XORToken): XORToken ={
    if ( (!xOne.value && xTwo.value) || (xOne.value && !xTwo.value) ) XORToken(true) else XORToken(false)
  }

  var _currentState: mutable.Map[String, String] = mutable.Map[String,String]()

  override def toString = "XOR\n" +
                          "CurrentState: " + _currentState

  override def currentOutput: Option[Seq[Token]] = _currentOutput

  override def currentState: mutable.Map[String, String] = _currentState
}

/**
 * Test run of XOR atomic model.
 */
object XORSimulationTest extends App {
  val sim = new Simulation(new XORModel, mutable.Seq[Seq[Token]](Seq[Token](XORToken(false), XORToken(true)), Seq[Token]()))
  sim.runSimulation()
}
