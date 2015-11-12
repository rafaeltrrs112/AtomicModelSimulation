package Asims454.a5.events

import Asims454.a5._
import scala.collection.mutable
/*
 * TODO Generalize for single generation networks.
 * TODO Create Router, RouterConfig for the current discrete event model system.
 * TODO Ensure that current system works with reflexive networks with Routers (influencers).
 * TODO Clean up code and make scala-docs for new code.
 */
object SimulationContext {
  //The universal priority queue used by the entire simulation.
  val priorityQueue = EventQueue(mutable.PriorityQueue[Event]())
}

/**
 * 
 * @param events
 */
case class EventQueue(var events : mutable.PriorityQueue[Event]){
  def dump : Unit = while (events.nonEmpty) {
    val nextEvent = events.dequeue()
    println(nextEvent.message)
    nextEvent.execute
  }
  def act : Unit = events.dequeue().execute
  def insert(e : Event) : Unit = events.enqueue(e)
  def insertAll(e : Seq[Event]) = e.foreach(this.insert)
}

/**
 * 
 */
trait Event extends Ordered[Event] {
  val priority : Int
  val message : String
  def execute : Unit
  def compare(that : Event) : Int =  that.priority - this.priority
}

/**
 * 
 * @param priority
 * @param message
 * @param issuer
 * @param context
 */
case class OutputEvent(override val priority : Int, override val message : String, issuer : DrillPress, context : EventQueue) extends Event {
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
        val spawnedInputEvent : InputEvent = InputEvent(priority, stampedInput, subscribedModel, "", SimulationContext.priorityQueue)
        println(stampedInput)
        spawnedInputEvent
      }
      val printToConsole : String = issuer.name + " outputs " + output.toString + " to " + issuer.subscribers.get.head.name
      context.insertAll(generatedInputEvents.toSeq)
    }
    else {
      //Call delta internal then update state print the output and time
      val currentQuery = priority
      issuer.queryTime = currentQuery
      val output = issuer.lambda(issuer._state)
      issuer._state = issuer.deltaInternal(issuer._state)
      println(issuer.name + " outputs " + output + " at " + priority)
    }
  }

  override def toString : String = {
    "OutputEvent("+priority+","+message+","+issuer+",contextSize:"+context.events.size+")"
  }
}

/**
 * 
 * @param priority
 * @param input
 * @param receiver
 * @param message
 * @param context
 */
case class InputEvent(override val priority : Int, input : DrillPressInput, receiver : DrillPress, override val message : String, context : EventQueue) extends Event {
  override def execute: Unit = {
    //Take the input and pass it into the receiver then create an outPut event for each of the disks.
    val start = priority + receiver.triggerTime
    val end = if(input.input.value == 1) start + receiver.triggerTime else (input.input.value * receiver.triggerTime) + receiver.triggerTime
    val timedParts = for(time <-  Range(start, end, receiver.triggerTime)) yield {
      input.input.parts.head.reStamp(stampTime = time)
    }

    val timedInput = DrillPressInput(priority, PartSet(mutable.PriorityQueue[Part]()))
    timedParts.foreach(timedInput.input.parts.enqueue(_))

    receiver._state = receiver.deltaExternal(receiver._state, timedInput, priority)

    val outputEvents = for(time <-  Range(start, end, receiver.triggerTime)) yield {
      val output : OutputEvent = OutputEvent(time, "", receiver, SimulationContext.priorityQueue)
      output
    }
    println(receiver.name + " receives " + timedInput + " at " + priority)
    SimulationContext.priorityQueue.insertAll(outputEvents.toSeq)
    receiver.lastEventTime = priority
  }
}

/**
 * 
 * @param oI
 * @param priority
 * @param message
 */
case class ConfluentEvent(oI : (OutputEvent, InputEvent), priority : Int, message : String) extends Event {
  def execute : Unit = {
    oI._1.execute
    oI._2.execute
  }
}
