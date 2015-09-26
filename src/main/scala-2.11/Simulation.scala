/**
 *
 */
class Simulation(model : Model[Coin], inputs : Seq[Seq[SimulationToken]]) {
  def runSimulation() : Unit = inputs.foreach(model.stateTransition)
}
