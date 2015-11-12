package raxsim.model

import Asims454.a2.{Quarter, Dime, Nickel, Coin}
import raxsim.io.{Output, Token}

//Implement and simulate an atomic model of a vending machine.
// The machine sells coffee and accepts only nickels, dimes, and quarters.
// When $1 has been inserted, the machine dispenses a cup of coffee.
// The machine accepts one coin at a time (note the difference from last time), and does not have a cancel button.
// If there is no input to the machine for two seconds after a previous input,
// it dispenses coffee(s) (if the value of the entered coins is sufficient),
// and also returns change.

import scala.collection.mutable

/**
 * Model trait for the framework.
 * @deprecated
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
trait State {
  def map : scala.collection.immutable.Map[String, String]
}

trait AtomicModel[-I <: Token, +O <: Token]{

  val name : String

  def output[C >: O] : Option[mutable.IndexedSeq[C]]

  def state: State

  def delta[C <: I](input: IndexedSeq[C])

}
//This is a trait because it is executed only by a Discrete Event Simulator. The simulator needs to know
//how to execute the model according to the inputs coming in.


case class DiscreteTimeInput[+I <: raxsim.io.InputOutput[Int]](second : Int, input : I)

object main extends App{

}