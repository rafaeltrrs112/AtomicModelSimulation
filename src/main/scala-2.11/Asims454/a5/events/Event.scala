/**
  * Assignment 5 : New features for the simulation framework.
  */
package Asims454.a5.events

import Asims454.a5._
import scala.collection.mutable

/**
 * The Context holding the priority queue(s) used by all simulations running during this instance.
 */
object SimulationContext {
  /**
    *   The universal priority queue used by the entire simulation.
    */
  val priorityQueue = EventQueue(mutable.PriorityQueue[Event]())
}

/**
 * The event queue handler class.
 * @param events
  *               A priority queue of events.
 */
case class EventQueue(var events : mutable.PriorityQueue[Event]){
  /**
    * Continuously spills out the contents of the queue until no more events are being registered in it.
    */
  def dump : Unit = while (events.nonEmpty) {
    val nextEvent = events.dequeue()
    val existingBindable = events.find(canJoin(nextEvent, _))
    if(existingBindable.isDefined){

      println("Confluent case occurring at time " + nextEvent.priority + " for Model["+nextEvent.name+"]");

      val orderedConfluent : (OutputEvent, InputEvent) = orderedConfluentPair(nextEvent, existingBindable.get)
      events = events.filterNot(_ == existingBindable.get)
      val confluentCase = ConfluentEvent({
        orderedConfluent._1.execute
        orderedConfluent._2.execute
      }, nextEvent.priority, orderedConfluent._1.name)
      confluentCase.execute
    } else{
      println(nextEvent.name)
      nextEvent.execute
    }
  }

  /**
    * Executes the next event in the priority queue.
    */
  def act : Unit = events.dequeue().execute

  /**
    * @param e
    *          The event being inserted into the priority queue.
    */
  def insert(e : Event) : Unit = {
    //If event is of type input get all input events with the same model and pass them to input events join method...
    events.enqueue(e)
  }

  /**
    * Inserts a sequence of events into the queue.
    * @param e
    *          Some traversable collection of events.
    */
  def insertAll(e : Traversable[Event]) = e.foreach(this.insert)

  /**
    * Defines whether one event due for execution is confluent with another event due for execution at the same time.
    * @param incoming
    *                 Returns a boolean if these non-confluent events can be joined together into
    *                 a confluent event.
    * @param existing
    *                 The event being compared from within the queue.
    * @return
    *         True if these two events can be joined.
    */
  def canJoin(incoming : Event, existing : Event) : Boolean = {
    incoming match {
      case OutputEvent(existing.priority, existing.name, _, _) => existing match {
        case inputEvent : InputEvent => true
        case _ => false
      }
      case InputEvent(existing.priority,_,_,existing.name,_) => existing match {
        case outputEvent : OutputEvent => true
        case _ => false
      }
      case _ => false
    }
  }

  /**
    * Joins two events together as a confluent pair.
    * @param incoming
    *                 An event due for execution.
    * @param existing
    *                 An event due for execution.
    * @return
    *         Both events bound together as their confluent case.
    */
  def orderedConfluentPair(incoming : Event, existing : Event) : (OutputEvent, InputEvent) = {
    incoming match {
      case outputEvent : OutputEvent => existing match {
        case inputEvent : InputEvent => (outputEvent, inputEvent)
      }
      case inputEvent : InputEvent => existing match {
        case outputEvent : OutputEvent => (outputEvent, inputEvent)
      }
      case _ => throw new AssertionError("Invalid incoming event of type : " + incoming.getClass.getSimpleName)
    }
  }
}

/**
 * Trait for events than can be placed in an event cue and excuted in some fashion.
 */
trait Event extends Ordered[Event] {
  /**
    * The real time this event is occuring.
    */
  val priority : Int
  /**
    * The name of the model making the output.
    */
  val name : String

  /**
    * Executes the event. Similar to run in runnable.
    */
  def execute : Unit

  /**
    * @inheritdoc
    */
  def compare(that : Event) : Int =  that.priority - this.priority
}

/**
  * Event of type output. Retrieves some output from the issuer model. If the receiver has any subscribers, input events are spawned with priorities
  * set at the expected output time.
  * @param priority
  *                 The real time this event is occurring.
  * @param name
  *                The name of the model making the output.
 * @param issuer
  *               The model outputting some value at this time.
 * @param context
  *                The context queue the event was spawned into. Any output events spawned by this event.
 */
case class OutputEvent(override val priority : Int, override val name : String, issuer : DrillPress, context : EventQueue) extends Event {
  /**
    * @inheritdoc
    * */
  def execute : Unit =  {
    //println("Parts count for :  " + issuer.name + " , is : " + issuer._state.parts.size)
    //If their are children of this model then branch out and create an input event for each one...
    if(issuer.subscribers.isDefined){
      //Call delta internal then update state print the output and time
      val currentQuery = priority
      issuer.queryTime = currentQuery
      //Create and input event for all the models subscribed to this one...
      val output = issuer.lambda(issuer._state)
      issuer._state = issuer.deltaInternal(issuer._state)
      val generatedInputEvents = for(subscribedModel <- issuer.subscribers.get) yield {
        val stampedInput : DrillPressInput = output.toInput(priority)
        val spawnedInputEvent : InputEvent = InputEvent(priority, stampedInput, subscribedModel, subscribedModel.name, SimulationContext.priorityQueue)
        println(stampedInput)
        spawnedInputEvent
      }
      context.insertAll(generatedInputEvents.toSeq)
    }
    else {
      //Call delta internal then update state print the output and time
      val currentQuery = priority
      issuer.queryTime = currentQuery
      println("Creating output at ... " + issuer.queryTime)
      val output = issuer.lambda(issuer._state)
      issuer._state = issuer.deltaInternal(issuer._state)
      println(issuer.name + " outputs " + output + " at " + priority)
    }
  }

  /**
    * The string representation of this class.
    * @return
    *         The string containing information about the event. The context queue is not outputted due to stack overflow.
    */
  override def toString : String = {
    "OutputEvent("+priority+","+name+","+issuer+",contextSize:"+context.events.size+")"
  }
}

/**
  * Event of type input. Inserts some input into a receiver. An output event is spawned at an interval with a step of the trigger time
  * for retrieving the inevitable output from this model later on in the event queue.
  * @param priority
  *                 The real time this event is occurring.
  * @param input
  *              The input into the receiver.
  * @param receiver
  *                 The model receiving the input.
  * @param name
  *                The name of the model receiving input.
  * @param context
  *                The context queue the event was spawned into. Any output events spawned by this event.
 */
case class InputEvent(override val priority : Int, input : DrillPressInput, receiver : DrillPress, override val name : String, context : EventQueue) extends Event {
  /**
    * @inheritdoc
    */
  override def execute : Unit = {
    //Take the input and pass it into the receiver then create an outPut event for each of the disks.
    val timeRange = receiver.getTicketSet(priority, input.input.value)
    val timedParts = for(time <- timeRange) yield {
      input.input.parts.head.reStamp(stampTime = time)
    }

    val timedInput = DrillPressInput(priority, PartSet(mutable.PriorityQueue[Part]()))
    timedParts.foreach(timedInput.input.parts.enqueue(_))

    receiver._state = receiver.deltaExternal(receiver._state, timedInput, priority)

    val outputEvents : IndexedSeq[OutputEvent] = for(time <- timeRange) yield {

    val output : OutputEvent = OutputEvent(time, receiver.name, receiver, SimulationContext.priorityQueue)
      output
    }
    println(receiver.name + " receives " + timedInput + " at " + priority)
    SimulationContext.priorityQueue.insertAll(outputEvents.toSeq)
    receiver.lastEventTime = priority

  }
}

/**
  * Creates a confluent event.
  */
object ConfluentEvent{
  def apply(confluentBlock : => Unit, priority : Int, name : String) : ConfluentEvent = new ConfluentEvent(confluentBlock, priority, name)
}

/**
  * Holds the by name block that defines what the confluent event is for for a specific model.
  * @param confluentBlock
  *                        The by block reference that executes the confluent case for a model.
  * @param priority
  *                 The real world time the event occurred.
  * @param name
  *                The message to print when this event occurs if any.
  */
class ConfluentEvent(confluentBlock : => Unit, override val priority : Int, override val name : String) extends Event {
  def execute : Unit = confluentBlock
}