import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * System Simulation Assignment 1
 * Vending Machine
 * */
object VendingMachine extends Model[Coin]{
  //Set the initial state
  var _currentState = new State(mutable.Map[String, ArrayBuffer[Coin]]())

  def fullCoins : ArrayBuffer[Coin] = quarters ++ nickels ++ dimes
  def vendingMachinePurse : Int = fullCoins.foldLeft(0)(_ + _.value)

  var playerPurse : Int = 0
  val coins = mutable.Map[String, ArrayBuffer[Coin]]()


  def currentState_=(stateInput : State[Coin]) : Unit = _currentState = stateInput
  def currentState = _currentState


  def quarters = coins(Coin.Quarter)
  def nickels = coins(Coin.Nickel)
  def dimes = coins(Coin.Dime)

  //Initial currency value
  coins += ("Nickel" -> ArrayBuffer[Coin](Nickel()))
  coins += ("Dime" -> ArrayBuffer[Coin](Dime()))
  coins += ("Quarter" -> ArrayBuffer[Coin](Quarter()))

  //Emptiness check methods
  def hasQuarter = coins(Coin.Quarter).nonEmpty
  def hasNickel = coins(Coin.Nickel).nonEmpty
  def hasDime = coins(Coin.Dime).nonEmpty


  /**
   *
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  override def stateTransition(input: SimulationToken*){
//    println(vendingMachinePurse)
    input.foreach {
      case coin : Coin => {
        coin match {
          case Nickel() => {
            playerPurse += coin.value
            coins(Coin.Nickel) += Nickel()
          }
          case Dime() => {
            playerPurse += coin.value
            coins(Coin.Dime) += Dime()
          }
          case Quarter() => {
            playerPurse += coin.value
            coins(Coin.Quarter) += Quarter()
          }
        }
      }
    }
//    println(vendingMachinePurse)
//    println("Nickels " + nickels.size + " * 5 = " + nickels.size * 5)
//    println("Dimes " + dimes.size + " * 10 = " + dimes.size * 10)
//    println("Quarters " + quarters.size + " * 25 = " + quarters.size * 25)
    processInput(input)
  }

  /**
   *
   * @return
   * Pair containing the output if any, and a state that will
   * always exits.
   */
  override def outputAndView: (Option[Output], State[Coin]) = {
    null
  }

  /**
   * State transition functions alters the state member of the
   * model. A helper method will be used to set this function
   * to the the one returned when makeTransitionFunction is called.
   * @param input
   * Input into the model.
   */
  def processInput(input: Seq[SimulationToken]): Unit = {
    println(playerPurse + " cents entered")
    val numberOfCoffees : Int = playerPurse / 100
    var previousState : State[Coin] = new State(currentState.stateMap)
    var nextState = {
      val dispenseCoffee : Boolean = playerPurse >= 100
      playerPurse = if(dispenseCoffee) playerPurse - numberOfCoffees * 100 else playerPurse
//      println("Your change: " + playerPurse + " cents")
      if(dispenseCoffee) println("Coffee dispensed: " + numberOfCoffees) else println("No coffee for you")
      if(dispenseCoffee){

        val changeReceived : (Option[Change], Option[Change], Option[Change]) = retrieveChange(playerPurse)
        println("You get " + changeReceived._1.getOrElse("No") + " quarters")
        println("You get " + changeReceived._2.getOrElse("No") + " dimes")
        println("You get " + changeReceived._3.getOrElse("No") + " nickels")
      }
      println("Machine money: " + vendingMachinePurse)
      println("Credit : " + playerPurse)
      val coffeeDispense : Seq[Coffee] = Seq.fill(numberOfCoffees)(Coffee())
      println("Dispensed Coffees : " + coffeeDispense.size + " \n" + coffeeDispense)
//      println(fullCoins)
    }
  }

  def retrieveChange(amount : Int) : (Option[Change], Option[Change], Option[Change]) = {
    println(amount)
    var amountOwed : Int = amount


    //Call by name
    def quartersOwed : Int = amountOwed / 25

    def dimeOwed : Int = amountOwed / 10
    def nickelOwed : Int = amountOwed / 5

    var quarterChangeOption : Option[Change] = None
    var dimeChangeOption : Option[Change] = None
    var nickelChangeOption : Option[Change] = None

    if(quartersOwed > 0){
      if(hasQuarter){
        if(quarters.size < quartersOwed){
          val quarterReturn = Seq.fill(quarters.size)(Quarter())
          quarters.clear()
          quarterChangeOption = Some(Change(quarterReturn))
          amountOwed -= quarters.size * 25
        }
        else {
          quarters.remove(0, quartersOwed )
          quarterChangeOption = Some(Change(Seq.fill(quartersOwed)(Quarter())))
          amountOwed -= quartersOwed * 25
        }
      }
    }


    if(dimeOwed > 0){
      if(hasDime){
        //I have some quarters
        if(dimes.size < dimeOwed){
          val dimeReturn = Seq.fill(dimes.size)(Dime())
          dimes.clear()
          dimeChangeOption = Some(Change(dimeReturn))
          amountOwed -= dimes.size * 10
        }
        //I have all the quarters needed
        else {
          dimes.remove(0, dimeOwed)
          dimeChangeOption = Some(Change(Seq.fill(dimeOwed)(Dime())))
          amountOwed -= dimeOwed * 10

        }
      }
    }

    if(nickelOwed > 0) {
      if (hasNickel) {
        if (nickels.size < nickelOwed) {
          val nickelReturn = Seq.fill(nickels.size)(Nickel())
          nickels.clear()
          nickelChangeOption = Some(Change(nickelReturn))
          amountOwed -= nickels.size * 5

        }
        //I have all the quarters needed
        else {
          nickels.remove(0, nickelOwed)
          nickelChangeOption = Some(Change(Seq.fill(nickelOwed)(Nickel())))
          amountOwed -= nickelOwed * 5
        }
      }
    }
    (quarterChangeOption, dimeChangeOption, nickelChangeOption)
  }

  /**
   * Some list of stateTransition functions that can be
   * retrieved from below.
   */
}
object VendingMachineTest extends App {
  VendingMachine.stateTransition(Quarter(), Quarter())
  VendingMachine.stateTransition(Quarter(), Quarter(), Quarter(), Quarter())
//  VendingMachine.stateTransition(Quarter(), Quarter(), Quarter(), Nickel(), Nickel(), Quarter())
//  VendingMachine.stateTransition(Nickel(), Dime(), Dime(), Quarter(), Nickel(), Quarter())

}