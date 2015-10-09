package raxsimulate.builders

import raxsimulate.io.{WrappedToken, EmptyToken, Token}
import raxsimulate.model.Model
import raxsimulate.network.{NetworkModel, ConfigMap, RoutedCluster}

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * A linker class for use within the network model implementation.
 */
class ClusterBuilder(outPuttingModel : Model){
  //Beta models do not receive output from outside their network.
  private val betaModels : mutable.Map[String, Model] = mutable.Map[String, Model]()

  //Alpha models receive the inputs from the networks source inputs
  private val alphaModels : mutable.Map[Model, IndexedSeq[Int]] = mutable.Map[Model, IndexedSeq[Int]]()

  private def modelsJoined : mutable.Map[String, Model] = {
    val alphaModelsMap = for(pair <- alphaModels) yield (pair._1.name, pair._1)
    alphaModelsMap ++ betaModels
  }

  def addModel(model : Model) = betaModels += ((model.name, model))

  def addModel(model : Map[String, Model]) = betaModels ++= model

  def addAlpha(model : Model, inputIDs : IndexedSeq[Int]) = alphaModels += ((model, inputIDs))


  private def bindRoutes(configMap : ConfigMap) = {
    val config = configMap.configuration

    val allModels = modelsJoined

    //Routed cluster needs to be filtered better
    val routedCluster = for(model <- allModels.values) yield {
      (model, allModels.filterNot( (pair) =>
        {
          if(config.contains(model.name)) config(model.name).contains(pair._1) else false
        }
      ).values)
    }

    //Here the cluster is cleansed of any bindings that have models outputting to themselves or
    //to any alphas.
    val correctedCluster = for((model, boundedModels) <- routedCluster) yield {
      (model, boundedModels.filterNot((e) => e.name.equals(model.name) || alphaModels.forall((pair) => pair._1.name.equals(e.name))))
    }
    correctedCluster.toMap
  }

  def generateNetwork(modelName : String, config : ConfigMap) : Option[NetworkModel] = {
    if (modelsJoined.isEmpty) None
    else Some(
      new NetworkModel(
        modelName,
        RoutedCluster(alphaModels, bindRoutes(config), outPuttingModel)
      )
    )
  }
}



