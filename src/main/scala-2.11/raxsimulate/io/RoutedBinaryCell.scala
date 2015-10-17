package raxsimulate.io

import raxsimulate.model.Model
import scala.collection.mutable

/**
 *
 * @param receiver
 *        The model receiving input from the neighbors
 * @param neighbors
 *                  The neighbors outputting to the receiver
 */
case class RoutedBinaryCell(receiver : BinaryModel, neighbors: (BinaryModel, BinaryModel))

/**
 *
 * @param route
 *
 */
class OneTenExecutor(route : IndexedSeq[RoutedBinaryCell]) {
  def execute() = {
    route.foreach((cell) => {
      val c = if (cell.receiver.currentOutput.head(0).asInstanceOf[OneTenToken].value == 0){
        " "
      } else {
        "\u25AE"
      }
      print(c)
    })
    println()

    val outputCollection = for(cell <- route) yield {
      val left = cell.neighbors._1
      val right = cell.neighbors._2
      OneTenWrap(left.currentOutput.get.head, right.currentOutput.get.head)
    }
    val outPutIterator = outputCollection.iterator
    route.foreach { (cell) => {
      cell.receiver.stateTransition(IndexedSeq(outPutIterator.next()))
    }
    }
  }
}

class BinaryModel(modelName : String, init : OneTenToken) extends Model {
  override def name: String = modelName

  override def currentState: mutable.Map[String, String] = {
    mutable.Map(("", "" ))
  }

  override def currentOutput: Option[IndexedSeq[Token]] = Some(mutable.IndexedSeq(_currentOutput))

  var _currentOutput : OneTenToken = init

  /**
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = {
    input.foreach{ (token) =>{
        token match
        {
          case OneTenWrap((left :OneTenToken, right: OneTenToken)) => {
            if(_currentOutput.value == 1){
              (left.value, right.value) match {
                case (0, 0) => _currentOutput = OneTenToken(1)
                case (0, 1) => _currentOutput = OneTenToken(1)
                case (1 ,0) => _currentOutput = OneTenToken(1)
                case (1, 1) => _currentOutput = OneTenToken(0)
              }
            }
            else if(_currentOutput.value == 0){
              (left.value, right.value) match {
                case (0, 0) => _currentOutput = OneTenToken(0)
                case (0, 1) => _currentOutput = OneTenToken(1)
                case (1 ,0) => _currentOutput = OneTenToken(0)
                case (1, 1) => _currentOutput = OneTenToken(1)
              }
            }
          }
        }
      }
      }

    }
  }
class CellWall(modelName : String, output: OneTenToken) extends BinaryModel(modelName, output) {
  override def name: String = modelName

  override def currentState: mutable.Map[String, String] = mutable.Map(("Wall", " default : " + output))

  override def currentOutput: Option[IndexedSeq[Token]] = Some(IndexedSeq(output))

  /**
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = {}
}


case class OneTenWrap(oneTenPair : (Token, Token)) extends Token

case class OneTenToken(value : Int) extends Token

object OneTenToken{
  def tuple2ToIndexedSeq[T](t: (T,T)): mutable.IndexedSeq[T] = mutable.IndexedSeq(t._1, t._2)
}
/*
 * Left most out of seven cell
 * The logic works. Now create a builder for making many cells and binding them into RoutedBinaryCells
 * that can then be plugged into a the executor..
 * TODO Create builder for automating the creation of RoutedBinaryCells.
 */
object test extends App {

}