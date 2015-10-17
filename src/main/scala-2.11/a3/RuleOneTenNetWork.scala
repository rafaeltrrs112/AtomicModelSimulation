package a3

import raxsimulate.io._

import scala.collection.mutable.ArrayBuffer

object CellGenerator{
  def generateCellNetwork(n : Int): IndexedSeq[RoutedBinaryCell] ={


    val cellList = for(i <- Range(0, n)) yield {
      if(i == 0 || i == n){
        new BinaryModel("O"+i, OneTenToken(0))
      }
      else if(i == (n / 2)){
        new BinaryModel("O"+i, OneTenToken(1))
      }
      else{
        new BinaryModel("O"+i, OneTenToken(0))
      }
    }

    for(ind <- cellList.indices) yield {
      if(ind == 0){
        RoutedBinaryCell(cellList(ind), (cellList.last, cellList(ind + 1)))
      }
      else if (ind == cellList.size - 1 ){
        RoutedBinaryCell(cellList(ind), (cellList(ind - 1), cellList.head))
      }
      else {
        RoutedBinaryCell(cellList(ind), (cellList(ind - 1), cellList(ind + 1)))
      }
    }
  }

}
object RuleOneTenNetWork extends App{

  val OneTenNetWork = new OneTenExecutor(CellGenerator.generateCellNetwork(175))
  for(i <- 0 to 10000){
    Thread.sleep(10)
    OneTenNetWork.execute()
  }
}
