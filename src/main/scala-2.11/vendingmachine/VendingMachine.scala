package vendingmachine

import raxsimulate.{Model, Token}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rtorres12 on 10/4/15.
 */
/*
 * System Simulation Assignment 1
 * Vending Machine
 * */
class VendingMachine extends Model {
  var cancel = Cancel(false)

  var _currentState: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()

  def currentState = _currentState

  var _currentOutput: Option[Seq[Token]] = None

  override def currentOutput = _currentOutput


  var currentInput = Seq[Token]()

  var playerChange = Seq[Change]()

  def vendingMachineCoins: ArrayBuffer[Coin] = quarters ++ nickels ++ dimes

  def vendingMachinePurse: Int = vendingMachineCoins.foldLeft(0)(_ + _.value)

  var playerPurse: Int = 0
  val coins = mutable.Map[String, ArrayBuffer[Coin]]()

  def quarters = coins(Coin.Quarter)

  def nickels = coins(Coin.Nickel)

  def dimes = coins(Coin.Dime)

  coins += ("Nickel" -> ArrayBuffer[Coin]())
  coins += ("Dime" -> ArrayBuffer[Coin]())
  coins += ("Quarter" -> ArrayBuffer[Coin]())

  var oweMoney: Boolean = false

  //Emptiness check methods
  def hasQuarter = coins(Coin.Quarter).nonEmpty

  def hasNickel = coins(Coin.Nickel).nonEmpty

  def hasDime = coins(Coin.Dime).nonEmpty

  /*
   * @param input
   * Takes in input and update internal state independent
   * variables. Calls transitionState() after input processing
   * to alter state dependent model properties and and transition
   * state.
   */
  override def stateTransition(input: Seq[Token]) {
    updatePurses(input)
    _currentState =
      mutable.Map[String, String]("Credit" -> playerPurse.toString,
        "Money in machine" -> vendingMachinePurse.toString,
        "Cancel" -> cancel.toString,
        "CurrentOutput" -> _currentOutput.toString,
        "Error" -> (if (oweMoney) "Cannot output change, enter more coins or please\n" +
          "call Sim Soda Company at 1-800-123-4567"
        else "None"))
    _currentOutput = None
    processInput(input)
  }

  def updatePurses(input: Seq[Token]): Unit = {
    input.foreach {
      case coin: Coin => {
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
      case c: Cancel => {
        cancel = c
      }
    }
  }

  override def toString: String = {
    _currentState.foldLeft("")(_ + "\n" + _.toString())
  }

  def updateOutput(n: Int): Unit = {
    _currentOutput = if (n > 0) Some(Seq.fill(n)(Coffee())) else None
  }

  def updateOutput(coins: Seq[Coin]): Unit = {
    _currentOutput = if (coins.nonEmpty) Some(coins) else None
  }

  /**
   * State transition functions alters the state member of the
   * model. A helper method will be used to set this function
   * to the the one returned when makeTransitionFunction is called.
   * @param input
   * Input into the model.
   */
  def processInput(input: Seq[Token]): Unit = {
    val numberOfCoffees: Int = playerPurse / 100
    updateOutput(numberOfCoffees)
    val dispenseCoffee: Boolean = playerPurse >= 100
    val currentPlayerPurse = if (!cancel.value && dispenseCoffee) playerPurse - numberOfCoffees * 100 else playerPurse
    playerPurse = currentPlayerPurse
    if (!cancel.value && dispenseCoffee) {
      updateOutput(numberOfCoffees)
    }
    else if (cancel.value) {
      val changeReceived = cancelEntered()
      //      changeReceived.flatMap(_.change)
      val testType = changeReceived.flatMap(_.change)
      if (!(testType.foldLeft(0)(_ + _.value) == playerPurse)) {
      }
      playerPurse = 0
      updateOutput(changeReceived.flatMap(_.change))
      changeReceived.foreach(_currentOutput ++ _.change)
    }
    cancel = Cancel(false)
  }

  def cancelEntered(): Seq[Change] = {
    val getChange = retrieveChange(playerPurse)
    val test: List[Change] = List(getChange._1, getChange._2, getChange._3).flatten
    test.toSeq
  }


  //User may or may not get change
  def retrieveChange(amount: Int): (Option[Change], Option[Change], Option[Change]) = {
    var amountOwed: Int = amount

    //Call by name
    def quartersOwed: Int = amountOwed / 25
    def dimeOwed: Int = amountOwed / 10
    def nickelOwed: Int = amountOwed / 5

    var quarterChangeOption: Option[Change] = None
    var dimeChangeOption: Option[Change] = None
    var nickelChangeOption: Option[Change] = None

    if (quartersOwed > 0) {
      if (hasQuarter) {
        //I have some quarters
        if (quarters.size < quartersOwed) {
          val quarterReturn = Seq.fill(quarters.size)(Quarter())
          quarters.clear()
          quarterChangeOption = Some(Change(quarterReturn))
          amountOwed -= quarters.size * 25
        }
        //I have all the quarters needed
        else {
          quarters.remove(0, quartersOwed)
          quarterChangeOption = Some(Change(Seq.fill(quartersOwed)(Quarter())))
          amountOwed -= quartersOwed * 25
        }
      }
    }

    if (dimeOwed > 0) {
      if (hasDime) {
        //I have some quarters
        if (dimes.size < dimeOwed) {
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

    if (nickelOwed > 0) {
      if (hasNickel) {
        //I have some quarters
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
    oweMoney = amountOwed > 0
    (quarterChangeOption, dimeChangeOption, nickelChangeOption)
  }
}
