package Asims454.a4

import Asims454.a2._
import Asims454.a4.components.io.Change
import Asims454.a4.components.VendingMachineState
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.List

/**
 * Singleton containing utility methods for dealing with the state mutations in the VendingMachine.
 */
object VendingUtils {

  /**
   * Utility method for joining the user's coins with the internal coins.
   * @param state
   *              A VendingMachineState.
   * @return
   *         A VendingMachine with all user coins added to the player's coins.
   */
  def joinCoins(state: VendingMachineState): VendingMachineState ={
    val allCoins : Change = state.internalCoins ++ state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))
    VendingMachineState(allCoins, None)
  }

  /**
   * 
   * @param amount
   *               The amount owed to the user of customer.
   * @param state
   *              A VendingMachineState.
   * @return
   *         A VendingMachineState with the coins owed to customer removed from the internal coins.
   */
  def generateOutputEventState(amount : Int, state : VendingMachineState): VendingMachineState = {

    var _state = joinCoins(state)
    def hasQuarter : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Quarter])
    }
    def hasNickel : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Nickel])
    }
    def hasDime : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Dime])
    }

    def clearQuarters(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 25)), Some(_state.userCoins.get))
    }
    def clearNickels(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 5)), Some(_state.userCoins.get))
    }
    def clearDimes(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 10)), Some(_state.userCoins.get))
    }

    def quarterCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 25) value + 1 else value
      }
    }
    def nickelCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 5) value + 1 else value
      }
    }
    def dimeCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 10) value + 1 else value
      }
    }

    def removeQuarters(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 25) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 25) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

    def removeNickels(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 5) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 5) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

    def removeDimes(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 10) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 10) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

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
        if (quarterCount < quartersOwed) {
          val quarterReturn : IndexedSeq[Coin] = collection.immutable.IndexedSeq.fill(quarterCount)(Quarter())
          clearQuarters()
          quarterChangeOption = Some(Change(quarterReturn))
          amountOwed -= quarterCount * 25
        }
        //I have all the quarters needed
        else {
          removeQuarters(quartersOwed)
          quarterChangeOption = Some(Change(IndexedSeq.fill(quartersOwed)(Quarter())))
          amountOwed -= quartersOwed * 25
        }
      }
    }

    if (dimeOwed > 0) {
      if (hasDime) {
        //I have some quarters
        if (dimeCount < dimeOwed) {
          val dimeReturn = IndexedSeq.fill(dimeCount)(Dime())
          clearDimes()
          dimeChangeOption = Some(Change(dimeReturn))
          amountOwed -= dimeCount * 10
        }
        //I have all the quarters needed
        else {
          removeDimes(dimeCount)
          dimeChangeOption = Some(Change(IndexedSeq.fill(dimeOwed)(Dime())))
          amountOwed -= dimeOwed * 10

        }
      }
    }

    if (nickelOwed > 0) {
      if (hasNickel) {
        //I have some quarters
        if (nickelCount < nickelOwed) {
          val nickelReturn = IndexedSeq.fill(nickelCount)(Nickel())
          clearNickels()
          nickelChangeOption = Some(Change(nickelReturn))
          amountOwed -= nickelCount* 5
        }
        //I have all the quarters needed
        else {
          removeNickels(nickelOwed)
          nickelChangeOption = Some(Change(IndexedSeq.fill(nickelOwed)(Nickel())))
          amountOwed -= nickelOwed * 5
        }
      }
    }
    VendingMachineState(_state.internalCoins, None)
  }

  /**
   * Utility method for getting the amount of change owed to the user.
   * @param amount
   *               The amount owed to the customer.
   * @param state
   *              A VendingMachineState.
   * @return
   *          The amount of changed owed to the user.
   */
  def getChange(amount: Int, state : VendingMachineState): List[Change] = {
    var _state = joinCoins(state)
    def hasQuarter : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Quarter])
    }
    def hasNickel : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Nickel])
    }
    def hasDime : Boolean  = {
      _state.internalCoins.change.exists((coin) => coin.isInstanceOf[Dime])
    }

    def clearQuarters(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 25)), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }
    def clearNickels(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 5)), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }
    def clearDimes(): Unit = {
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 10)), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

    def quarterCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 25) value + 1 else value
      }
    }
    def nickelCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 5) value + 1 else value
      }
    }
    def dimeCount : Int = {
      _state.internalCoins.change.foldLeft(0){ (value, coin) =>
        if (coin.value == 10) value + 1 else value
      }
    }

    def removeQuarters(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 25) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 25) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

    def removeNickels(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 5) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 5) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

    def removeDimes(removeCount : Int) : Unit = {
      val quarters = scala.collection.mutable.ArrayBuffer[Coin]()
      for(i <- _state.internalCoins.change if i.value == 10) quarters += i
      quarters.remove(0, removeCount)
      _state = VendingMachineState(Change(_state.internalCoins.change.filter((coin) => coin.value != 10) ++ quarters), Some(_state.userCoins.getOrElse(Change(collection.immutable.IndexedSeq(NoCoin())))))
    }

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
        if (quarterCount < quartersOwed) {
          val quarterReturn : IndexedSeq[Coin] = collection.immutable.IndexedSeq.fill(quarterCount)(Quarter())
          clearQuarters()
          quarterChangeOption = Some(Change(quarterReturn))
          amountOwed -= quarterCount * 25
        }
        //I have all the quarters needed
        else {
          removeQuarters(quartersOwed)
          quarterChangeOption = Some(Change(IndexedSeq.fill(quartersOwed)(Quarter())))
          amountOwed -= quartersOwed * 25
        }
      }
    }

    if (dimeOwed > 0) {
      if (hasDime) {
        //I have some quarters
        if (dimeCount < dimeOwed) {
          val dimeReturn = IndexedSeq.fill(dimeCount)(Dime())
          clearDimes()
          dimeChangeOption = Some(Change(dimeReturn))
          amountOwed -= dimeCount * 10
        }
        //I have all the quarters needed
        else {
          removeDimes(dimeCount)
          dimeChangeOption = Some(Change(IndexedSeq.fill(dimeOwed)(Dime())))
          amountOwed -= dimeOwed * 10

        }
      }
    }

    if (nickelOwed > 0) {
      if (hasNickel) {
        //I have some quarters
        if (nickelCount < nickelOwed) {
          val nickelReturn = IndexedSeq.fill(nickelCount)(Nickel())
          clearNickels()
          nickelChangeOption = Some(Change(nickelReturn))
          amountOwed -= nickelCount* 5
        }
        //I have all the quarters needed
        else {
          removeNickels(nickelOwed)
          nickelChangeOption = Some(Change(IndexedSeq.fill(nickelOwed)(Nickel())))
          amountOwed -= nickelOwed * 5
        }
      }
    }
    List(quarterChangeOption, dimeChangeOption, nickelChangeOption).flatten
  }

}
