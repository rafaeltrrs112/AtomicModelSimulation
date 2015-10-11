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
    route.foreach { (cell) => {      println(cell.receiver.name + " outputting " + cell.receiver.currentOutput.get(0))
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
  val O1 : BinaryModel = new BinaryModel("O1", OneTenToken(0))
  val O2 : BinaryModel = new BinaryModel("O2", OneTenToken(0))
  val O3 : BinaryModel = new BinaryModel("O3", OneTenToken(0))
  val O4 : BinaryModel = new BinaryModel("O4", OneTenToken(0))
  val O5 : BinaryModel = new BinaryModel("O5", OneTenToken(0))
  val O6 : BinaryModel = new BinaryModel("O6", OneTenToken(0))
  val O7 : BinaryModel = new BinaryModel("O7", OneTenToken(0))
  val O8 : BinaryModel = new BinaryModel("O8", OneTenToken(0))
  val O9 : BinaryModel = new BinaryModel("O9", OneTenToken(0))
  val O10 : BinaryModel = new BinaryModel("10", OneTenToken(0))
  val O11: BinaryModel = new BinaryModel("O11", OneTenToken(0))
  val O12 : BinaryModel = new BinaryModel("O12", OneTenToken(0))
  val O13 : BinaryModel = new BinaryModel("O13", OneTenToken(0))
  val O14 : BinaryModel = new BinaryModel("O14", OneTenToken(0))
  val O15 : BinaryModel = new BinaryModel("O15", OneTenToken(0))
  val O16 : BinaryModel = new BinaryModel("O16", OneTenToken(1))
  val O17 : BinaryModel = new BinaryModel("O17", OneTenToken(0))
  val O18 : BinaryModel = new BinaryModel("O18", OneTenToken(0))
  val O19 : BinaryModel = new BinaryModel("O19", OneTenToken(0))
  val O20 : BinaryModel = new BinaryModel("O20", OneTenToken(0))
  val O21 : BinaryModel = new BinaryModel("O21", OneTenToken(0))
  val O22 : BinaryModel = new BinaryModel("O22", OneTenToken(0))
  val O23 : BinaryModel = new BinaryModel("O23", OneTenToken(0))
  val O24 : BinaryModel = new BinaryModel("O24", OneTenToken(0))
  val O25 : BinaryModel = new BinaryModel("O25", OneTenToken(0))
  val O26 : BinaryModel = new BinaryModel("O26", OneTenToken(0))
  val O27 : BinaryModel = new BinaryModel("O27", OneTenToken(0))
  val O28 : BinaryModel = new BinaryModel("O28", OneTenToken(0))
  val O29 : BinaryModel = new BinaryModel("O29", OneTenToken(0))
  val O30 : BinaryModel = new BinaryModel("O30", OneTenToken(0))
  val O31 : BinaryModel = new BinaryModel("O31", OneTenToken(0))
  val O32 : BinaryModel = new BinaryModel("O32", OneTenToken(0))
  val O33 : BinaryModel = new BinaryModel("O33", OneTenToken(0))
  val O34 : BinaryModel = new BinaryModel("O34", OneTenToken(0))
  val O35 : CellWall = new CellWall("35", OneTenToken(0))

  val routeOne = RoutedBinaryCell(O1, (O0, O2))
  val routeTwo = RoutedBinaryCell(O2, (O1, O3))
  val routeThree = RoutedBinaryCell(O3, (O2, O4))
  val routeFour = RoutedBinaryCell(O4, (O3, O5))
  val routeFive = RoutedBinaryCell(O6, (O5, O7))
  val routeSix = RoutedBinaryCell(O7, (O6, O8))
  val routeSeven = RoutedBinaryCell(O8, (O7, O9))
  val routeEight = RoutedBinaryCell(O9, (O8, O10))
  val routeNine = RoutedBinaryCell(O10, (O9, O11))
  val routeTen = RoutedBinaryCell(O11, (O10, O12))
  val routeEleven = RoutedBinaryCell(O12, (O11, O13))
  val routeTwelve = RoutedBinaryCell(O13, (O12, O14))
  val routeThirteen = RoutedBinaryCell(O14, (O13, O15))
  val routeFourteen = RoutedBinaryCell(O15, (O14, O16))
  val routeFifteen = RoutedBinaryCell(O16, (O15, O17))
  val routeSixteen = RoutedBinaryCell(O17, (O16, O18))
  val routeSeventeen = RoutedBinaryCell(O18, (O17, O19))
  val routeEighteen = RoutedBinaryCell(O19, (O18, O20))
  val routeNineteen = RoutedBinaryCell(O20, (O19, O21))
  val routeTwenty = RoutedBinaryCell(O21, (O20, O22))
  val routeTwentyOne = RoutedBinaryCell(O22, (O21, O23))
  val routeTwentyTwo = RoutedBinaryCell(O23, (O22, O24))
  val routeTwentyThree = RoutedBinaryCell(O24, (O23, O25))
  val routeTwentyFour = RoutedBinaryCell(O25, (O24, O26))
  val routeTwentyFive = RoutedBinaryCell(O26, (O25, O27))
  val routeTwentySix = RoutedBinaryCell(O27, (O26, O28))
  val routeTwentySeven = RoutedBinaryCell(O28, (O27, O29))
  val routeTwentyEight = RoutedBinaryCell(O29, (O28, O30))
  val routeTwentyNine = RoutedBinaryCell(O30, (O29, O31))
  val routeThirty = RoutedBinaryCell(O31, (O30, O32))
  val routeThirtyOne = RoutedBinaryCell(O32, (O31, O33))
  val routeThirtyTwo = RoutedBinaryCell(O33, (O32, O34))
  val routeThirtyThree = RoutedBinaryCell(O34, (O33, O35))


  val OneTenNetWork = new OneTenExecutor(IndexedSeq(routeOne, routeTwo, routeThree, routeFour, routeFive, routeSix, routeSeven, routeEight, routeNine,
  routeTen, routeEleven, routeTwelve, routeThirteen, routeFourteen, routeFifteen, routeSixteen, routeSeventeen, routeEighteen, routeNineteen, routeTwenty,
  routeTwentyOne, routeTwentyTwo, routeTwentyThree, routeTwentyFour, routeTwentyFive, routeTwentySix, routeTwentySeven, routeTwentyEight, routeTwentyNine,
  routeThirty, routeThirtyOne, routeThirtyTwo, routeThirtyThree))
  for(i <- 0 to 100) OneTenNetWork.execute()
}