package VendingMachine

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rtorres12 on 10/4/15.
 */
object VendingMachineTest extends App {


  val inputStream: ArrayBuffer[ArrayBuffer[Token]] = ArrayBuffer(
    ArrayBuffer[Token](Quarter(), Quarter(), Quarter(), Cancel(false), Dime(), Dime(), Dime()),
    ArrayBuffer[Token](Cancel(true)), ArrayBuffer[Token](Cancel(false)))


  val inputStreamTwo: ArrayBuffer[ArrayBuffer[Token]] = ArrayBuffer(
    ArrayBuffer[Token](Quarter(), Quarter(), Quarter(), Quarter(), Cancel(false), Dime(), Dime(), Dime(), Nickel()),
    ArrayBuffer[Token](Cancel(true)), ArrayBuffer[Token](Cancel(false)))

  println("***Run Two***")
  val simulation = new Simulation(new VendingMachine(), inputStream)
  simulation.runSimulation()

  println("***Run Two***")
  val simulationTwo = new Simulation(new VendingMachine(), inputStreamTwo)
  simulationTwo.runSimulation()
}
