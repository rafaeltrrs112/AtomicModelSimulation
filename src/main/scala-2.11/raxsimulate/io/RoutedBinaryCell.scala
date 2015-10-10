package raxsimulate.io

import raxsimulate.model.Model

import scala.collection.immutable.IntMap.Bin
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
    route.foreach { (cell) => {
      val left = cell.neighbors._1
      val right = cell.neighbors._2
      val output = OneTenWrap(left.currentOutput.get.head, right.currentOutput.get.head)
      cell.receiver.stateTransition(IndexedSeq(output))
    }
    }
    route.foreach { (cell) => {      println(cell.receiver.name + " outputting " + cell.receiver.currentOutput)
    } }
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
            if (_currentOutput.value == 0){
              if(left.value == 1){
                if(right.value == 1){
                  _currentOutput = OneTenToken(1)
                }
                else if(right.value == 0){
                  _currentOutput = OneTenToken(0)
                }
              }
              else if(left.value == 0){
                if(right.value == 1){
                  _currentOutput = OneTenToken(1)
                }
                else if(right.value == 0){
                  _currentOutput = OneTenToken(0)
                }
              }
            }
            else if(_currentOutput.value == 1){
              if(right.value == 1){
                if(left.value == 1 ){
                  _currentOutput = OneTenToken(0)
                }
                else if(left.value == 0){
                  _currentOutput = OneTenToken(1)
                }
              }
              else if(right.value == 0){
                if(left.value == 1 ){
                  _currentOutput = OneTenToken(1)
                }
                else if(left.value == 0){
                  _currentOutput = OneTenToken(1)
                }
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
object main extends App{
  val O0 : CellWall = new CellWall("O0", OneTenToken(0))
  val O1 : BinaryModel = new BinaryModel("O1", OneTenToken(1))
  val O2 : BinaryModel = new BinaryModel("O2", OneTenToken(0))
  val O3 : BinaryModel = new BinaryModel("O3", OneTenToken(1))
  val O4 : BinaryModel = new BinaryModel("O4", OneTenToken(0))
  val O5 : BinaryModel = new BinaryModel("O5", OneTenToken(1))
  val O6 : CellWall = new CellWall("O6", OneTenToken(0))

  val routeOne = RoutedBinaryCell(O1, (O0, O2))
  val routeTwo = RoutedBinaryCell(O2, (O1, O3))
  val routeThree = RoutedBinaryCell(O3, (O2, O4))
  val routeFour = RoutedBinaryCell(O4, (O3, O5))
  val routeFive = RoutedBinaryCell(O5, (O4, O6))
  val OneTenNetWork = new OneTenExecutor(IndexedSeq(routeOne, routeTwo, routeThree, routeFour, routeFive))
  for(i <- 0 to 100) OneTenNetWork.execute()
}