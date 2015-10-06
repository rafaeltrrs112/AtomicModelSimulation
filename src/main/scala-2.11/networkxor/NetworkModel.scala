package networkxor

import raxsimulate.Model

import scala.collection.immutable
/**
 *
 */
class NetworkModel {

}

object NetWorkModel{
  //Create builder object here
}

/**
 * The configuration map contains the output to model configuration for the network model.
 * The key is the name of the model outputting some data. And the value is the sequence of the names
 * of the models whose input is the output of the outputting model.
 * @param configMap
 *                    Key is output model name. Value is the name of the models receive the input.
 */
case class NetWorkConfig(configMap : immutable.Map[String, Seq[String]])

/**
 *
 * @param model
 * @param config
 */
class NetWorkModelBuilder(model : Model, config : NetWorkConfig){

}

//Different types of configuration objects that contain routing functions
trait Router