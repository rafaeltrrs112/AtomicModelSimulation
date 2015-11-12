package Asims454.a3

import raxsim.Simulation
import raxsim.builders.ClusterBuilder
import raxsim.io.ModelRouteConfig
import raxsim.model.{InputBuffer, Router}
import raxsim.network.ConfigMap

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

  //A cluster is a precursor to a network model.
  //A cluster of bounded models exists within every network
  //The cluster builder requires in it's constructor the
  //outputting model.
  val xorCluster = new ClusterBuilder(O2)

  //An alpha model takes in input from the outside only
  //The XOR Router takes in all the inputs and returns two.
  //The two outputs at every state are passed to 02
  xorCluster.addAlpha(xorOneRouted, IndexedSeq(0, 1, 2))
  //Beta models take in input from other models
  xorCluster.addModel(O2)

  //A config map consists of the model outputting
  //with a value of strings containing the names of all the models who
  //will be receiving the output from the key model.
  val xorBindings = ConfigMap(Map(("01_Routed", IndexedSeq("O2"))))

  //The inner networks cluster is now complete. A network can now be built
  //out of that cluster assuming all inputs are handed to alphas or routed through
  //and some model is bound to the output wire.
  val N2 = xorCluster.generateNetwork("N2", xorBindings).getOrElse(
    throw new RuntimeException("Cannot generate model")
  )

  //We can now create a simple application specific model for
  //this homework network.
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
