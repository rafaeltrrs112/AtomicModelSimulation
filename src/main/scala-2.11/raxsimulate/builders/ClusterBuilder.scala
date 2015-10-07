package raxsimulate.builders

import raxsimulate.io.{WrappedToken, EmptyToken, Token}
import raxsimulate.model.Model
import raxsimulate.network.{ConfigMap, RoutedCluster}

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * A linker class for use within the network model implementation.
 * @param inputRange
 *                  The maximum amount of inputs
 */
class ClusterBuilder(inputRange : Int){
  //Beta models do not receive output from outside their network.
  private val betaModels : mutable.Map[String, Model] = mutable.Map[String, Model]()

  //Alpha models receive the inputs from the networks source inputs
  private val alphaModels : mutable.Map[Model, Seq[Int]] = mutable.Map[Model, Seq[Int]]()

  private def modelsJoined : mutable.Map[String, Model] = {
    val alphaModelsMap = for(pair <- alphaModels) yield (pair._1.name, pair._1)
    alphaModelsMap ++ betaModels
  }

  def addModel(model : Model) = betaModels += ((model.name, model))

  def addModel(model : Map[String, Model]) = betaModels ++= model

  def addAlpha(model : Model, inputIDs : Seq[Int]) = alphaModels += ((model, inputIDs))

  private def bindRoutes(configMap : ConfigMap) = {

    val config = configMap.configuration
    var routedModelMap = mutable.Map[Model, Seq[Model]]()

    val allModels = modelsJoined

    val routedCluster = for(model <- allModels.values) yield {
      (model, allModels.filter(_._2.equals(config(model.name))).values)
    }

    routedCluster.toMap
  }

  def generateNetwork(config : ConfigMap) : Option[RoutedCluster] = {
    if (modelsJoined.isEmpty) None else Some(RoutedCluster(alphaModels, bindRoutes(config)))
  }
}
class Router(modelName : String) extends Model{
  var routedModels : mutable.ArrayBuffer[RoutedModel] = mutable.ArrayBuffer[RoutedModel]()
  override def name: String = modelName

  override def currentState: mutable.Map[String, String] = mutable.Map(("CurrentOutPuts", currentOutput.toString))

  override def currentOutput: Option[Seq[Token]] = Some(_currentOutput.toSeq)
  val _currentOutput : ArrayBuffer[Token] = ArrayBuffer[Token]()

  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   *
   */
  override def stateTransition(input: Seq[Token]): Unit = {

    _currentOutput.clear()

    //Iterate through the routed model case class and collect the inputs for the
    //models according to the config.
    for((RoutedModel(model, indices)) <- routedModels){
      val currentInputs : Seq[Token] = for(ind <- indices) yield input(ind)
      model.stateTransition(currentInputs)

      //Outputs are wrapped into a wrapped token so outputs can be packaged out
      //in the order of their indices
      _currentOutput += WrappedToken(currentOutput.getOrElse(EmptyToken.emptyTokenSeq))
    }
  }

  def addRoutedModel(config : RoutedModel) : Unit = {
    routedModels += config
  }
}

/**
 * Routed Models must be inserted into a router in the order of their output index 
 * out of the router. I.E. if added first with inputIndices(0,1,2) inputs I(0,1,2) -> O(0)
 * @param model
 *              The model for routing.
 * @param inputIndices
 *                     The indices of the inputs.
 */
case class RoutedModel(model : Model, inputIndices : Seq[Int])
