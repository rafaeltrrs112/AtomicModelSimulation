/**
 *
 */
class Simulation(model : Model, inputs : Seq[Seq[SimulationToken]]) {
  var tickNumber = 0
  def runSimulation() : Unit = inputs.foreach((v) => {
    print("Tick [" + tickNumber + "]")
    model.stateTransition(v)
    println(model)
    tickNumber += 1
    println
  })
}
