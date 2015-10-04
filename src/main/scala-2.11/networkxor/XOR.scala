package networkxor

import raxsimulate.{Simulation, Model, Token}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class XorToken(value : Boolean) extends Token
/**
 * Assignment 3 network xor.XOR model.
 */
class XORModel extends Model {

  override var _currentOutput: Option[Seq[Token]] =
    Some(ArrayBuffer[XorToken](XorToken(true)))

  /**
   *
   * @param input
   * List of XOR tokens to process. True only if odd number of inputs are true
   *
   */
  override def stateTransition(input: Seq[Token]): Unit = {
    val result : Seq[XorToken] = {
      input.map(_.asInstanceOf[XorToken])
    }
    _currentOutput = Some(Seq[XorToken](result.foldLeft(XorToken(false))(XOR)))
    _currentState("Output: ") = _currentOutput.toString
  }

  private def XOR(xOne : XorToken, xTwo : XorToken): XorToken ={
    if ( (!xOne.value && xTwo.value) || (xOne.value && !xTwo.value) ) XorToken(true) else XorToken(false)
  }

  override var _currentState: mutable.Map[String, String] = mutable.Map[String,String]()

  override def toString = "XOR\n" +
                          "CurrentState: " + _currentState
}
object XORSimulationTest extends App {
  val sim = new Simulation(new XORModel, mutable.Seq[Seq[Token]](Seq[Token](XorToken(false), XorToken(true)), Seq[Token]()))
  sim.runSimulation()
}
