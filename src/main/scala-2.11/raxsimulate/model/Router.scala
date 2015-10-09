package raxsimulate.model

import raxsimulate.io.{ModelRouteConfig, EmptyToken, Token, WrappedToken}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 *
 * @param modelName
 */
class Router(modelName : String) extends Model{
  var configRoutes : mutable.ArrayBuffer[ModelRouteConfig] = mutable.ArrayBuffer[ModelRouteConfig]()
  override def name: String = modelName

  override def currentState: mutable.Map[String, String] = mutable.Map(("CurrentOutPuts", currentOutput.toString))

  //Consists of a sequence of wrapped tokens. Any Model that may be receiving wrapped token inputs
  //should unwrap every token it receives to check if it may be a wrapped token and unravel it.
  override def currentOutput: Option[IndexedSeq[Token]] = Some(_currentOutput.toIndexedSeq)
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
  override def stateTransition(input: IndexedSeq[Token]): Unit = {

    _currentOutput.clear()

    //Iterate through the routed model case class and collect the inputs for the
    //models according to the config.
    for((ModelRouteConfig(model, indices, _)) <- configRoutes){
      println("Router has inputs" + input +" attempting to send to "+model.name)
      val currentInputs : IndexedSeq[Token] = for(ind <- indices) yield input(ind)
       model.stateTransition(currentInputs)

      //Outputs are wrapped into a wrapped token so outputs can be packaged out
      //in the order of their indices
      _currentOutput += WrappedToken(model.currentOutput.getOrElse(EmptyToken.emptyTokenIndexedSeq))
    }
    //Append the outputs not designated as a routed index to the end of the _currentOutput
    val allIndices = configRoutes.foldLeft(IndexedSeq(0))((IndexedSeq, route) => route.inputIndices ++ IndexedSeq)
    val passThroughTokens :IndexedSeq[Token] = for(index <- input.indices if !allIndices.contains(index)) yield input(index)

   _currentOutput += WrappedToken(passThroughTokens)
  }

  def addRoutedModel(config : ModelRouteConfig) : Unit = {
    configRoutes += config
  }
  
}
