package Asims454.a5

import Asims454.a4.components.event.Alarm
import Asims454.a5.events._
import raxsim.io.InputOutput
import raxsim.model._
import raxsim.model.{DiscreteTimeInput, DiscreteEventModel}

import scala.collection.mutable

/**
 * The state of the drill/press at any given time
 */
import scala.collection.immutable.{Map, Queue}
import scala.collection.mutable.PriorityQueue

sealed trait Part extends raxsim.io.Token with InputOutput[Int] with Ordered[Part] {
  val value : Int
  val stampTime : Int
  def reStamp(stampTime : Int ) : Part
  override def toString = {
    value match {
      case 0 => "Ball"
      case 1 => "Disk"
      case 2 => "Washer"
    }
  }
  def compare(that : Part) : Int =  that.stampTime - this.stampTime

  def process : Part
}

case class Ball(override val stampTime : Int, override val value : Int = 0) extends Part {
  override def process: Part = Disk(stampTime + 120)
  override def reStamp(stampTime : Int ) : Part = Ball(stampTime)
}
case class Disk(override val stampTime : Int, override val value : Int = 1) extends Part {
  override def process: Part = Washer(stampTime + 120)
  override def reStamp(stampTime : Int ) : Part = Disk(stampTime)

}
case class Washer(override val stampTime : Int, override val value : Int = 2) extends Part {
  override def process: Part = Washer(stampTime + 120)
  override def reStamp(stampTime : Int ) : Part = throw new AssertionError("Washer's cannot be processed further")

}

case class PartSet(parts : collection.mutable.PriorityQueue[Part]) extends InputOutput[Int]{
  override def value: Int = parts.size
}

/**
 * An input class wrapper for a drill or press model.
 * @param second
 *               Deprecated
 * @param input
 *               A set of parts being inputted into a drill or press model.
 */
class DrillPressInput(override val second : Int, override val input : PartSet) extends DiscreteTimeInput[PartSet](second, input)

object DrillPressInput {
  def apply(second : Int, input : PartSet) : DrillPressInput = new DrillPressInput(second, input)
}

case class DrillPressOutput(part : Part) extends InputOutput[Int]{
  override def value: Int =
  part match {
    case Ball(_,_) => 0
    case Disk(_,_) => 1
    case Washer(_,_) => 2
  }

  def toInput(stampTime : Int ) : DrillPressInput = {
    val input = value match {
      case 0 => Ball(stampTime)
      case 1 => Disk(stampTime)
      case 2 => Washer(stampTime)
    }
    DrillPressInput(stampTime, PartSet(mutable.PriorityQueue(input)))
  }
}

//TODO add to the State trait a join method that returns a new state...essentially a lambda for the state
case class DrillPressState(var seconds : Int, parts : mutable.PriorityQueue[Part]) extends State {
  val DefaultSeconds = 0
  override def map: Map[String, String] = scala.collection.immutable.Map[String, String](s"next" ->  s"$seconds", "unprocessed" -> parts.size.toString)
  val size : Int = parts.size
  def join(partSet : PartSet, elapsedTime : Int)  =  {
    val joinedParts : mutable.PriorityQueue[Part] = parts ++ partSet.parts
    val newDiscreteTime : Int = this.seconds + elapsedTime

    DrillPressState(newDiscreteTime, joinedParts)
  }
  def reduced() : DrillPressState = {
    parts.dequeue()
    DrillPressState(DefaultSeconds, parts)
  }
}

class AlarmClock(val triggerTime : Int){
  var timeSet : Int = 0

  def reset(currentTime : Int) = {
    timeSet = currentTime
  }

  def isRinging(currentTime : Int) : Boolean = currentTime - timeSet > triggerTime
}

class DrillPress(var defaultState : DrillPressState, override val name : String) extends DiscreteEventModel[DrillPressInput, DrillPressOutput, DrillPressState, Part, DrillPressState]{
  val subscribers : Option[collection.immutable.Set[DrillPress]] = None
  override var triggerTime: Int = 120
  //don't user lastEventTime
  override var lastEventTime: Int = 0
  var queryTime : Int = 0
  /*
   * TODO, deprecate, the state holds the discrete time, the real time an output is produced is handled from the outside.
   * */
  var _state : DrillPressState = defaultState

  /**
   * The current state of the model.
   * @return
   * The state.
   */
  override def state: DrillPressState = _state

  /**
   * The external state transition function called when input is received from the outside.
   * @param state
   * The current state of the model.
   * @param realWorldTime
   * Elapsed time from this input to the last time an input was entered, if this method is being called the elapsed time
   * is assumed to be less than trigger time.
   * @param input
   * Some input that @implemented @DiscreteTimeInput
   * @tparam C
   * The type of input.
   */
  override def deltaExternal[C <: DrillPressInput](state: DrillPressState, input: C, realWorldTime : Int): DrillPressState = {
    //calculate elapsed time.
    val elapsedTime = realWorldTime - lastEventTime
    /*
     * Delta external takes in the current state of the machine, and input into the machine, and the elapsed time since an input was
     * last entered in to the machine
     *
     * Logic -> A new state is created by calling join on the state which takes in an input and some elapsed time and creates a new state
     * with the inputs added to the machines input pool, and the internal time of the machine increase by the value of the elapsed time.
     *
     * The elapsed time plus the state's time MUST BE LESS THAN THE TRIGGER TIME -> DELTA EXTERNAL SHOULD NOT BE CALLED FOR ANY TYPE OF
     * EVENT OTHER THAN INTERNAL STATE AGGREGATION...OUTPUT IS HANDLED BY INTERNAL...
     */
    if(elapsedTime > triggerTime) throw new InvalidDeltaCallException("Delta called at or past " + triggerTime)
    state.join(input.input , elapsedTime)
  }

  /**
   * The internal state transition function. Whose input is the curried output of
   * deltaExt. Is called when the internal event timer goes off.
   *
   * @param state
   * Some input returned by @deltaExt.
   * @return
   * The new State of the model.
   */
  override def deltaInternal(state : DrillPressState): DrillPressState = {

    /*
     * Delta internal takes in the current state of the machine, and determines from that state what output has been produced and
     * how the creation of that output alters the state.
     *
     * For example : If the time is over two seconds then obviously the machine has outputted a part and the internal part queue should be reduced
     * by one and the internal elapsed time should be reset...The machine should never have to worry about real world time...
     * all it cares about is 0 .. 2...nothing more.
     *
     * Only call delta internal if the elapsed time is greater than or equal to the trigger time. If this call has been made throw an
     * exception.
     */
    state.reduced()
  }

  /**
   * Called when the timer has gone off and input is being entered.
   *
   * @param input
   * Some input that implemented DiscreteTimeInput
   * @param elapsedTime
   * The elapsed time computed from the clock time.
   * @param lastEventTime
   * The last time a system event
   * @tparam C
   * The type of input.
   */
  override def deltaConfluent[C <: DrillPressInput](input: C, elapsedTime: Int, lastEventTime: Int): DrillPressState = {
    null
  }
  /**
   * Creates output according to the current state.
   *
   * @tparam C
   * The type of output.
   * @return
   * An @Option of @Output
   */
  override def lambda[C >: DrillPressOutput](state: DrillPressState): C = {
    if(state.parts.head.stampTime != queryTime) {
      throw new InvalidLambdaCallException("Lambda called before " + triggerTime + " TRIGGER_TIME")
    }
    DrillPressOutput(state.parts.head.process)
  }

  /**
   * @param input
   * An input into the model.
   * @deprecated @param realWorldTime
   *                Do not use...
   * @tparam C
   * The type of input.
   * @return
   *        The new state of the model after input is received.
   */
  override def receive[C <: DrillPressInput](input: C, elapsedTime : Int): DrillPressState = {
    null
  }

  /**
   * Runs some check to determine if the time advance function should trigger. If so
   * the trigger may be triggered in this method before as it will be return as the
   * time advance value here. If not then and INF time will be released from timeAdvance.
   * @return
   * True if the advance clause has been met. False if not
   */
  override def advanceClause(): Boolean = _state match {
    case DrillPressState(_, parts) => parts.nonEmpty
  }
}

case class InvalidDeltaCallException(message : String) extends Exception {
  override def toString = message
}

case class InvalidLambdaCallException(message : String) extends Exception {
  override def toString = message
}

object Simulation extends App {
  val EventQueue = {
    new EventQueue(mutable.PriorityQueue[Event]())
  }

  val press = new DrillPress(DrillPressState(0, mutable.PriorityQueue[Part]()), "Press Prototype")
  val drill = new DrillPress(DrillPressState(0, mutable.PriorityQueue[Part]()), "Drill Prototype")


  val input = DrillPressInput(20, PartSet(mutable.PriorityQueue(Disk(0), Disk(0), Disk(0), Disk(0))))
  //println(Seq[Disk](Disk(), Disk()).size)
  InputEvent(20, input, drill, drill.name + " receiving batch input ", Context.priorityQueue).execute
  Context.priorityQueue.dump
  //Context.priorityQueue
  //println(drill.triggerTime)
}
