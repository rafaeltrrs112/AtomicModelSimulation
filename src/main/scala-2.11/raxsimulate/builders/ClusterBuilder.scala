package raxsimulate.builders

import networkxor.RoutedCluster
import raxsimulate.model.Model

import scala.collection.immutable.Map
import scala.collection.mutable

/**
 * A linker class for use within the network model implementation.
 * @param inputRange
 *                  The maximum amount of inputs
 */
class ClusterBuilder(inputRange : Int){
  //Beta models do not receive output from outside their network.
  private val betaModels : mutable.Map[String, Model] = mutable.Map[String, Model]()

  //Alpha models receive the inputs from the networks source inputs
  private val alphaModels : mutable.Map[Model, Set[Int]] = mutable.Map[Model, Set[Int]]()

  private def modelsJoined : mutable.Map[String, Model] = {
    val alphaModelsMap = for(pair <- alphaModels) yield (pair._1.name, pair._1)
    alphaModelsMap ++ betaModels
  }

  def addModel(model : Model) = betaModels += ((model.name, model))

  def addModel(model : Map[String, Model]) = betaModels ++= model

  def addAlpha(model : Model, inputIDs : Set[Int]) = alphaModels += ((model, inputIDs))

  private def bindRoutes(configMap : ConfigMap) = {
    val config = configMap.configuration
    var routedModelMap = mutable.Map[Model, Set[Model]]()

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
