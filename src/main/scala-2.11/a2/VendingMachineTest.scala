package a2

import raxsim.Simulation
import raxsim.io.Token

import scala.collection.mutable.ArrayBuffer

/**
 * Demo shown to Professor Pantaleev in class. Worked smoothly.
 */
object VendingMachineTest extends App {

  val inputStream: ArrayBuffer[ArrayBuffer[Token]] = ArrayBuffer(
    ArrayBuffer[Token](Quarter(), Quarter(), Quarter(), Cancel(false), Dime(), Dime(), Dime()),
    ArrayBuffer[Token](Cancel(true)), ArrayBuffer[Token](Cancel(false)))


  val inputStreamTwo: ArrayBuffer[ArrayBuffer[Token]] = ArrayBuffer(
    ArrayBuffer[Token](Quarter(), Quarter(), Quarter(), Quarter(), Cancel(false), Dime(), Dime(), Dime(), Nickel()),
    ArrayBuffer[Token](Cancel(true)), ArrayBuffer[Token](Cancel(false)))

  println("***Run Two***")
  val simulation = new Simulation(new VendingMachine("Starbucks Machine"), inputStream)
  simulation.runSimulation()

  println("***Run Two***")
  val simulationTwo = new Simulation(new VendingMachine("Starbucks Machine"), inputStreamTwo)
  simulationTwo.runSimulation()
}
