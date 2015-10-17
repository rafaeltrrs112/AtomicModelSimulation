//Note that all models have initial outputs. If at any time the output of a model is None it is
//assumed that all of models linked to this model are designed to handle an instance of None at
//any time in the simulation.
package raxsim.network

import raxsim.io.{EmptyToken, Token}
import raxsim.model.Model

import scala.collection.immutable.Map
import scala.collection.mutable
class NetworkModel(modelName : String, cluster: RoutedCluster) extends Model {

  override def name: String = modelName

  override def currentState: mutable.Map[String, String] = mutable.Map(("Output: ", currentState.toString()))

  override def currentOutput: Option[IndexedSeq[Token]] = cluster.bottomModel.currentOutput

  /*
   * Non-external/intra cluster inputs are insert into all non alpha models.
   *
   * TODO May deprecate none outputs in the future...not sure yet.
   */
  private def transitionCluster() : Unit = {
    for((model, linkedModels) <- cluster.clusterRoute){
      linkedModels.foreach{ (modelLink) => {
        modelLink.stateTransition(model.currentOutput.getOrElse(EmptyToken.emptyTokenIndexedSeq))
      }
      }
    }
  }

  /*
   * External inputs are inserted into the alpha models before any call on the rest of the cluster can occur.
   * If alpha models do not receive input and the clusters are transitioned a gap of null or corrupted data wil
   * propagate through the system.
   * @param inputs
   *               A set of input tokens.
   */
  private def alphaConsume(inputs : IndexedSeq[Token]) : Unit = {
    for((alpha, idSet) <- cluster.alphaRoute) {
      val validInputs : IndexedSeq[Token] = for(i <- idSet) yield inputs(i)
      alpha.stateTransition(validInputs)
    }
    transitionCluster()
  }

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   *
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = alphaConsume(input)
}

/**
 * The configuration map contains the output to model configuration for the network model.
 * The key is the name of the model outputting some data. And the value is the IndexedSequence of the names
 * of the models whose input is the output of the outputting model.
 * @param configuration
 *        Key is output model name. Value is the name of the models receive the input.
 */
case class ConfigMap(configuration : Map[String, IndexedSeq[String]])

/**
 * Map with model values associated with an iterable containing all models receiving input from the
 * key model.
 * @param alphaRoute
 *                   Map with model keys associated with it's respective raw input values. Each integer in the
 *                   value set corresponds to inputs coming in from outside the encapsulating model.
 * @param clusterRoute
 *                   A fully configured route map with outputting models as keys and all models receiving their input
 *                   are stored in the integer collection value associated with the model key.
 */
case class RoutedCluster(alphaRoute : mutable.Map[Model, IndexedSeq[Int]], clusterRoute : Map[Model, Iterable[Model]], bottomModel : Model)
