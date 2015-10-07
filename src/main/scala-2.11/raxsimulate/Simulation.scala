package raxsimulate

import raxsimulate.io.Token
import raxsimulate.model.Model

/**
 *
 */
class Simulation(model: Model, inputs: Seq[Seq[Token]]) {

  var tickNumber = 0

  def runSimulation(): Unit = inputs.foreach((v) => {

    println("Tick [" + tickNumber + "]")
    println("Model Output: " + model.currentOutput)

    model.stateTransition(v)

    tickNumber += 1

    println(model)
  })
}
