package networkxor

import raxsimulate.model.Model

import scala.collection.immutable.Map
import scala.collection.mutable
/**
 *
 */
class NetworkModel {

}

object NetWorkModel{

}

case class NetWorkConfig()


/**
 * The configuration map contains the output to model configuration for the network model.
 * The key is the name of the model outputting some data. And the value is the sequence of the names
 * of the models whose input is the output of the outputting model.
 * @param configuration
 *        Key is output model name. Value is the name of the models receive the input.
 */
case class ConfigMap(configuration : Map[String, Seq[String]])

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
case class RoutedCluster(alphaRoute : mutable.Map[Model, Set[Int]], clusterRoute : Map[Model, Iterable[Model]])

class Router{

}