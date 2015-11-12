package Asims454.a4

import Asims454.a2.{NoCoin, Dime, Quarter}
import Asims454.a4.components.{VendingMachineModel, OutputBank}
import Asims454.a4.components.io.{VendingMachineOutput, VendingMachineInput}
import scala.collection.mutable

object DiscreteExecutor extends App {
  def testExecute : Unit = {
    var realWorldTime = 0
    val model = new VendingMachineModel("vender")
    def execute(inputs : IndexedSeq[VendingMachineInput]): Unit = {
      for (input <- inputs) {
        realWorldTime = realWorldTime + input.second
        model._state = model.receive(input, input.second)
        OutputBank.bank.enqueue((realWorldTime, model.lambda(model._state)))
        model.lastEventTime = realWorldTime
      }
    }
    val inputs = IndexedSeq[VendingMachineInput](VendingMachineInput(1, Quarter()), VendingMachineInput(1, Quarter()),
    VendingMachineInput(1, Quarter()), VendingMachineInput(1, Quarter()), VendingMachineInput(2, Dime()),
    VendingMachineInput(1, Dime()), VendingMachineInput(1, Dime()), VendingMachineInput(7, NoCoin()))
    execute(inputs)
  }

  testExecute
  val outputs :  mutable.Buffer[(Int, components.VendingMachineOutput)]  =  OutputBank.bank.toBuffer
  println(outputs.filter(_._2.isDefined))

}
