package Asims454.a5

import Asims454.a5.events._
import raxsim.io.InputOutput
import raxsim.model.{DiscreteEventModel, DiscreteTimeInput, _}

import scala.collection.mutable

/**
  * Traits describing parts that can be entered into a machine.
  */
sealed trait Part extends raxsim.io.Token with InputOutput[Int] with Ordered[Part] {
  /**
    * The integer tag value of this token.
    */
  val value: Int

  /**
    * The time to stamp on this token.
    */
  val stampTime: Int

  /**
    *
    * @param stampTime
    *                  The time to stamp on the part.
    * @return
    *         A part with that time stamped on it.
    */
  def reStamp(stampTime: Int): Part

  /**
    *
    * @inheritdoc
    */
  override def toString = {
    value match {
      case 0 => "Ball"
      case 1 => "Disk"
      case 2 => "Washer"
    }
  }

  /**
    *
    * @inheritdoc
    */
  def compare(that: Part): Int = that.stampTime - this.stampTime

  /**
    * Return the type of part that this part can be processed to.
    * @return
    *          This part's processed version.
    */
  def process: Part
}

/**
  * A ball that can be inserted into a press.
  * @param stampTime
  *                  The time stamped on this part.
  * @param value
  *              This part's tag value.
  */
case class Ball(override val stampTime: Int, override val value: Int = 0) extends Part {
  override def process: Part = Disk(stampTime + 120)

  override def reStamp(stampTime: Int): Part = Ball(stampTime)
}

/**
  * A disk that can be inserted into a drill.
  * @param stampTime
  *                  The time stamped on this part.
  * @param value
  *              This part's tag value.
  */
case class Disk(override val stampTime: Int, override val value: Int = 1) extends Part {
  override def process: Part = Washer(stampTime + 120)

  override def reStamp(stampTime: Int): Part = Disk(stampTime)

}

/**
  * Washer outputted by a drill.
  * @param stampTime
  *                  The time stamped on this part.
  * @param value
  *              This part's tag value.
  */
case class Washer(override val stampTime: Int, override val value: Int = 2) extends Part {
  override def process: Part = Washer(stampTime + 120)

  override def reStamp(stampTime: Int): Part = throw new AssertionError("Washer's cannot be processed further")

}

/**
  * A set of parts held in the state of a machine.
  * @param parts
  *              The parts contained by this set.
  */
case class PartSet(parts: collection.mutable.PriorityQueue[Part]) extends InputOutput[Int] {
  override def value: Int = parts.size
}

/**
  * An input class wrapper for a drill or press model.
  * @param second
 *      Deprecated.
  * @param input
 * A set of parts being inputted into a drill or press model.
  */
class DrillPressInput(override val second: Int, override val input: PartSet) extends DiscreteTimeInput[PartSet](second, input)

/**
  * The input into a drill or press.
  */
object DrillPressInput {
  def apply(second: Int, input: PartSet): DrillPressInput = new DrillPressInput(second, input)
}

/**
  * The output of a drill or press.
  * @param part
  *             A part processed by a machine.
  */
case class DrillPressOutput(part: Part) extends InputOutput[Int] {
  /**
    * Determine part value.
    * @return
    *         The value of this part.
    */
  override def value: Int =
    part match {
      case Ball(_, _) => 0
      case Disk(_, _) => 1
      case Washer(_, _) => 2
    }

  /**
    * Converts this output into a type that can be inputted into another machine.
    * @param stampTime
    *                  The time to stamp on this part.
    * @return
    *         An input created from this output.
    */
  def toInput(stampTime: Int): DrillPressInput = {
    val input = value match {
      case 0 => Ball(stampTime)
      case 1 => Disk(stampTime)
      case 2 => Washer(stampTime)
    }
    DrillPressInput(stampTime, PartSet(mutable.PriorityQueue(input)))
  }
}

/**
  * The state of a drill or press. Contains the currently unprocessed parts.
  * @deprecated @param seconds
  *                The real world time known by this machine.
  * @param parts
  *              The parts contained in this machines bag.
  */
case class DrillPressState(var seconds: Int, parts: mutable.PriorityQueue[Part]) extends State {
  val DefaultSeconds = 0

  override def map: Map[String, String] = scala.collection.immutable.Map[String, String](s"next" -> s"$seconds", "unprocessed" -> parts.size.toString)

  val size: Int = parts.size

  /**
    * Helper method for delta external methods.
    * @param partSet
    *                A set of parts.
    * @param elapsedTime
    *                    The time elapsed since last input.
    * @return
    *         Returns the new state joined with the input coming in. Used by delta external usually.
    */
  def join(partSet: PartSet, elapsedTime: Int) = {
    val joinedParts: mutable.PriorityQueue[Part] = parts ++ partSet.parts
    val newDiscreteTime: Int = this.seconds + elapsedTime

    DrillPressState(newDiscreteTime, joinedParts)
  }

  /**
    * Helper method for delta internal to reduce the queue by one part.
    * @return
    *         A reduced state assuming output (delta internal transition) call.
    */
  def reduced(): DrillPressState = {
    parts.dequeue()
    DrillPressState(DefaultSeconds, parts)
  }
}

/**
  * A machine that takes two seconds to process an input.
  * @param defaultState
  *                     The default state of the machine
  * @param name
  *             The name of the machine.
  */
class DrillPress(var defaultState: DrillPressState, override val name: String) extends DiscreteEventModel[DrillPressInput, DrillPressOutput, DrillPressState, Part, DrillPressState] {
  /**
    * The children/mode's subsribed to this model. Used as an alternative to the network builder.
    */
  var subscribers: Option[collection.immutable.Set[DrillPress]] = None

  /**
    * The trigger time for this model's internal clock.
    */
  override var triggerTime: Int = 120

  /**
    * @deprecated
    */
  override var lastEventTime: Int = 0

  /**
    * Time stamp for checking if a valid call is being made from outside the model. Used to
    * determine when and how to throw exceptions.
    */
  var queryTime: Int = 0

  /**
    * The variable holding the current state value.
    */
  var _state: DrillPressState = defaultState

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
   *  Elapsed time from this input to the last time an input was entered, if this method is being called the elapsed time
    * is assumed to be less than trigger time.
    * @param input
   * Some input that @implemented @DiscreteTimeInput
    * @tparam C
   * The type of input.
    */
  override def deltaExternal[C <: DrillPressInput](state: DrillPressState, input: C, realWorldTime: Int): DrillPressState = {
    //calculate elapsed time.
    val elapsedTime = realWorldTime - lastEventTime
    /*
     * Delta external takes in the current state of the machine, and input into the machine, and the elapsed time since an input was
     * last entered in to the machine
     */
    state.join(input.input, elapsedTime)
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
  override def deltaInternal(state: DrillPressState): DrillPressState = {
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
    * Creates tickets for input.
    * @param insertTime
    *                   The time some number of inputs are inserted.
    * @param numberInputs
    *                     The number of inputs inserted.
    * @return
    *         A set of tickets for all inputs inserted.
    */
  def getTicketSet(insertTime : Int, numberInputs : Int) : IndexedSeq[Int] = {
    if(_state.parts.nonEmpty){
      val lateness = Math.abs(insertTime - _state.parts.last.stampTime)
      val firstOutputTime = lateness + insertTime + triggerTime

      val lastOutputTime = firstOutputTime + (triggerTime * (numberInputs - 1))

      val rangeValue = for(time <- firstOutputTime to  lastOutputTime by triggerTime) yield {
        time
      }

      rangeValue

    }
    else {
      val firstOutputTime = insertTime + triggerTime
      val lastOutputTime = triggerTime * numberInputs + insertTime
      val rangeValue = for(time <- firstOutputTime to lastOutputTime by 120) yield {
        time
      }
      rangeValue
    }
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
    if (state.parts.head.stampTime != queryTime) {
      //throw new InvalidLambdaCallException("Lambda called before " + triggerTime + " TRIGGER_TIME")
    }
    DrillPressOutput(state.parts.head.process)
  }

  /**
    * @param input
   * An input into the model.
    * @deprecated @param realWorldTime
    *             Do not use...
    * @tparam C
   * The type of input.
    * @return
   * The new state of the model after input is received.
    */
  override def receive[C <: DrillPressInput](input: C, elapsedTime: Int): DrillPressState = {
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

/**
  * An exception to throw when an invalid delta call is made from outside the model.
  * @param message
  *                The message to print.
  */
case class InvalidDeltaCallException(message: String) extends Exception {
  override def toString = message
}

/**
  * An exception to throw when an invalid lambda call is made from outside the model.
  * @param message
  *                The message to print.
  */
case class InvalidLambdaCallException(message: String) extends Exception {
  override def toString = message
}

object Simulation extends App {

  val EventQueue = {
    new EventQueue(mutable.PriorityQueue[Event]())
  }

  val press = new DrillPress(DrillPressState(0, mutable.PriorityQueue[Part]()), "Press Prototype")
  val drill = new DrillPress(DrillPressState(0, mutable.PriorityQueue[Part]()), "Drill Prototype")

  press.subscribers = Some(Set(drill))

  //Initial input into the drill
  val pressInputs = DrillPressInput(20, PartSet(mutable.PriorityQueue(Ball(0), Ball(0), Ball(0), Ball(0))))

  //Input into the drill to test the confluent case.
  val pressInputConf = DrillPressInput(140, PartSet(mutable.PriorityQueue(Ball(0), Ball(0))))

  //Insert both events into the queue.
  SimulationContext.priorityQueue.insert(InputEvent(20, pressInputs, press, press.name, SimulationContext.priorityQueue))
  SimulationContext.priorityQueue.insert(InputEvent(140, pressInputConf, press, press.name, SimulationContext.priorityQueue))

  //Begin the simulation by executing all events until finish.
  SimulationContext.priorityQueue.dump
}
