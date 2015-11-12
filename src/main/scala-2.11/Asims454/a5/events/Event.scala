package Asims454.a5.events

import Asims454.a5._
import raxsim.io.InputOutput

import scala.collection.mutable
import raxsim.model.{DiscreteTimeInput, DiscreteEventModel}

object Context{
  //The universal priority queue used by the entire simulation.
  val priorityQueue = EventQueue(mutable.PriorityQueue[Event]())
}


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

trait Event extends Ordered[Event] {
  val priority : Int
  val message : String
  def execute : Unit
  def compare(that : Event) : Int =  that.priority - this.priority
}

//Call's the curried lambda event and delta internal only...
case class OutputEvent(override val priority : Int, override val message : String, issuer : DrillPress, context : EventQueue) extends Event {
  def execute : Unit =  {
    //If their are children of this model then branch out and create an input event for each one...
    if(issuer.subscribers.isDefined){
      //Call delta internal then update state print the output and time
      val currentQuery = priority
      issuer.queryTime = currentQuery
      val output = issuer.lambda(issuer._state)
      issuer._state = issuer.deltaInternal(issuer._state)
      println(issuer.name + " outputs " + output + " at " + priority)
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

//Call's delta external only.
case class InputEvent(override val priority : Int, input : DrillPressInput, receiver : DrillPress, override val message : String, context : EventQueue) extends Event {
  override def execute: Unit = {
    println("in execute")
    //Take the input and pass it into the receiver then create an outPut event for each of the disks.
    val range = priority + receiver.triggerTime until input.input.value * receiver.triggerTime + receiver.triggerTime
    val timedParts = for(time <-  range by receiver.triggerTime) yield {
      input.input.parts.head.reStamp(stampTime = time)
    }

    val timedInput = DrillPressInput(20, PartSet(mutable.PriorityQueue[Part]()))
    timedParts.foreach(timedInput.input.parts.enqueue(_))

    receiver._state = receiver.deltaExternal(receiver._state, timedInput, priority)
    println(receiver._state)

    val items = for(time <-  range by receiver.triggerTime) yield {
      val output : OutputEvent = OutputEvent(time, "", receiver, Context.priorityQueue)
      output
    }
    Context.priorityQueue.insertAll(items.toSeq)
    println(items)
    receiver.lastEventTime = priority
  }
}

//Call's delta confluent -> lambda -> delta-internal -> delta-external
case class ConfluentEvent(oI : (OutputEvent, InputEvent), priority : Int, message : String) extends Event {
  def execute : Unit = {
    oI._1.execute
    oI._2.execute
  }
}
