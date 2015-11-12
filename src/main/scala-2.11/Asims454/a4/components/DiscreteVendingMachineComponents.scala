package Asims454.a4.components

import Asims454.a2.{Quarter, Coin}
import Asims454.a4.VendingUtils
import Asims454.a4.components.event._
import Asims454.a4.components.io.{VendingMachineInput, ChangeBuilder, Change, Coffee}
import raxsim.io.{InputOutput, Token}
import raxsim.model.{DiscreteEventModel, State}

import scala.collection.immutable._
import scala.collection.mutable

object OutputBank {
  val bank : mutable.Queue[(Int, VendingMachineOutput)] = mutable.Queue[(Int, VendingMachineOutput)]()
}

/**
 * The output of a VendingMachine
 * @param coffee
 *               An option of Coffee
 * @param change
 *               An option of Coin
 */
case class VendingMachineOutput(coffee : Option[Seq[Coffee]], change : Option[List[Change]]) extends InputOutput[Int]{
  /**
   * The value of the coins.
   * @return
   *         The value of the coins if any coins are outputted, if not then 0.
   */
  def value = if(change.isDefined) 0 else change.get.foldLeft(0)(_ + _.value)
  
  override def toString = {
    val coffeeString : String = if(coffee.isDefined) "Coffee(s) : " + coffee.get.foldLeft("")(_ + _.value) else "No Coffee."
    val changeString : String = if(change.isDefined){
      if(change.get.isEmpty) "No change." else change.get.foldLeft("")(_ + _.toString)
    }
    else "No change."
    "VendingMachineOutput : " + coffeeString + " " + changeString
  }

  /**
   * A utility method for determining if this output contains any actual output.
   * @return
   *         True if some output is present.
   */
  def isDefined : Boolean = coffee.isDefined || change.isDefined
}

/**
 *
 * @param internalCoins
 *                       The coins owned by the VendingMachine
 * @param userCoins
 *                  An option of Coin. Some value if the user has entered any coins, else None.
 */
case class VendingMachineState(internalCoins : Change, userCoins : Option[Change]) extends State {
  /**
   * The total value of the user's coins, else None.
   * @return
   *         The value of the user's coin.
   */
  def userValue : Int = if (userCoins.isEmpty) 0 else userCoins.get.change.foldLeft(0)(_ + _.value)

  /**
   * The total value of the Coins held by the vending machines.
   * @return
   *         The value of the VendingMachine's coins.
   */
  def internalValue : Int = internalCoins.change.foldLeft(0)(_ + _.value)

  /**
   * A Map of strings representing this state.
   * @return
   *         A Map of the internal values of this state.
   */
  override def map: scala.collection.immutable.Map[String, String] = {
    scala.collection.immutable.Map(
      ("Internal Coins", internalCoins.toString)         , ("User Coins", userCoins.toString),
      ("Internal Value", internalValue.toString)         , ("User value", userValue.toString)
    )
  }

  override def toString = map.foldLeft("VendingMachineState : ")((prev, kv)  => prev + kv._1 + " " + kv._2 + " ")
}

/**
 * A model implementation of a VendingMachine.
 * @param name
 *             The name of this Model
 * @param _state
 *               The default state of this VendingMachine
 */
class VendingMachineModel(override val name : String, var _state : VendingMachineState = VendingMachineState.DefaultState)
  extends DiscreteEventModel[VendingMachineInput, VendingMachineOutput, VendingMachineState, Change, VendingMachineState]{
  /**
   * Trigger time function that may be set to some dynamic time or
   * made dynamic according to other dependencies in the system.
   * @return
   * The least amount of time until the next event is triggered.
   */
  override var triggerTime : Int = 2
  override var lastEventTime: Int = 0
  override def state: VendingMachineState = _state

  //An alarm object for encapsulating events.
  val alarm = Alarm(triggerTime)

  //Implicit elapsed time for the alarm
  implicit var elapsedTime : Int = 0

  override def lambda[C >: VendingMachineOutput](state : VendingMachineState) : VendingMachineOutput = {
    println("Lambda called at elapsed time : " + elapsedTime)
    if(alarm.ringing){
      val coffees : Int = _state.userValue / 100
      println("coffees to receive " + coffees)
      if(coffees > 0){
        val amountOwed = state.userValue - coffees * 100
        val currentChange = VendingUtils.getChange(amountOwed, state)
        val coffeeOutput : collection.immutable.IndexedSeq[Coffee] = for(i <- 0 until coffees) yield Coffee()
        VendingMachineOutput(Some(coffeeOutput), Some(currentChange))
      }
      else {
        val amountOwed = state.userValue
        val currentChange = VendingUtils.getChange(amountOwed, state)
        VendingMachineOutput(None, Some(currentChange))
      }
    }
    else{
      VendingMachineOutput(None, None)
    }
  }

  override def advanceClause() : Boolean = {
    _state.userValue > 0
  }

  override def receive[C <: VendingMachineInput](input : C, realWorldTime : Int) : VendingMachineState = {

    //Entry is inserted with elapsed time before last input by the outside...
    elapsedTime = input.second

    if(alarm.triggerTime != alarm.INFINITE){
      if(elapsedTime >= alarm.triggerTime){
        //Alarm is ringing and input is being entered do confluent.
        println("Sending to confluent")
        deltaConfluent(input, elapsedTime, lastEventTime)
      }
      else {
        //alarm is set but no ringing.
        deltaExternal(state, input, elapsedTime)
      }
    }
    else deltaExternal(state, input, elapsedTime)
  }

  override def deltaExternal[C <: VendingMachineInput](state: VendingMachineState, input: C, elapsedTime : Int) : VendingMachineState = {
    alarm.set(2)
    val newUserCoins: Change = if (state.userCoins.isDefined) state.userCoins.get + input.input else new Change(IndexedSeq(input.input))
    increaseUserCoins(newUserCoins)
  }


  override def deltaInternal(input: VendingMachineState) : VendingMachineState = {
    alarm.clear
    elapsedTime = 0
    println("Joining coins together " + VendingUtils.joinCoins(state))
    VendingUtils.generateOutputEventState(state.userValue % 100, VendingUtils.joinCoins(state))
  }

  override def deltaConfluent[C <: VendingMachineInput](input: C, elapsedTime: Int, lastEventTime: Int): VendingMachineState = {
    println("The alarm has been triggered...calling both deltaInternal deltaExternal with a user value of..." + state.userValue)
    val output = lambda(state)
    println("Last time an event happened " + lastEventTime)
    OutputBank.bank.enqueue((lastEventTime + 2,  output))
    _state = VendingUtils.generateOutputEventState(state.userValue % 100, state)
    println("After input has been logged... the state is..." + state)
    deltaExternal(deltaInternal(state), input, 0)
  }

  /**
   * Helper method to aid with adding coins to the state.
   * @param userChange
   *                   The change to be added to the state's user coins.
   * @return
   *         The new state with @param userChange added to it's user coins.
   */
  def increaseUserCoins(userChange : Change): VendingMachineState = {
    VendingMachineState(_state.internalCoins, Some(userChange))
  }

}
object VendingMachine {
  val DefaultOutput : VendingMachineOutput = new VendingMachineOutput(None, None)
}
object VendingMachineState {
  val DefaultState : VendingMachineState = {
    VendingMachineState(ChangeBuilder(5, 5, 5 , 25, 25, 25, 25, 25, 5, 5, 10 , 10 ,10, 10, 10, 10), Some(ChangeBuilder(0)))
  }
}






