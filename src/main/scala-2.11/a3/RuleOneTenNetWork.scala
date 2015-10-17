package a3

import raxsim.io._


object RuleOneTenNetWork extends App {

  val OneTenNetWork = new OneTenExecutor(CellGenerator.generateCellNetwork(175))
  for(i <- 0 to 10000){
    Thread.sleep(10)
    OneTenNetWork.execute()
  }
}
