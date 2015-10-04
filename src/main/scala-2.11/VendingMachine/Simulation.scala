package VendingMachine

/**
 *
 */
class Simulation(model: Model, inputs: Seq[Seq[Token]]) {
  var tickNumber = 0

  def runSimulation(): Unit = inputs.foreach((v) => {
    println("\n\n" + "Tick [" + tickNumber + "]")
    println(model.currentOutput)
    model.stateTransition(v)
    tickNumber += 1
    println(model) // prints out the state of the model
  })
}
