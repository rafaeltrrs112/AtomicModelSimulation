package a3

import raxsimulate.Simulation
import raxsimulate.builders.ClusterBuilder
import raxsimulate.io.ModelRouteConfig
import raxsimulate.model.{InputBuffer, Model, Router}
import raxsimulate.network.ConfigMap

import scala.collection.immutable.Map

/**
 * First create a RoutedModel for the O1 Xor.
 * Then build a cluster with the RoutedModel O1 taking in all inputs
 */
object XorNetwork extends App {

  //The two XOR models that do some calculations and the memory model
  //used for recursion
  val O1inner = new XORModel("O1_Raw")
  val O2 = new XORModel("O2")
  val memoryModel = new InputBuffer("M1", XORToken(false), XORToken(false))

  //The router to encapsulate 01's input and on of the incoming
  //inputs and pass through (OUT(O1) and I2)
  //The configuration for xorOneRouted saying the first two input go to XorOne
  //the rest go out.
  //there should be a discrete way to set the pass through inputs as
  val xorOneRouted = new Router("O1_Routed")
  val routeConfig = ModelRouteConfig(O1inner, IndexedSeq(0, 1))
  xorOneRouted.addRoutedModel(routeConfig)

  val xorCluster = new ClusterBuilder(2, O2)
  
  xorCluster.addAlpha(xorOneRouted, IndexedSeq(0, 1, 2))
  xorCluster.addModel(O2)
  val xorBindings = ConfigMap(Map(("01_Routed", IndexedSeq("O2"))))
  
  val N2 = xorCluster.generateNetwork("N2", xorBindings).getOrElse(
    throw new UnsupportedOperationException("Cannot generate model")
  )

  //Now do the same...except this time the

  val N1 = new RecursiveNetwork("N1", N2, memoryModel)
  val simulator = new Simulation(
    N1,
    IndexedSeq(
      XORToken(false, true),
      XORToken(false, false),
      XORToken(true, false)
    )
  )
  simulator.runSimulation()
}
