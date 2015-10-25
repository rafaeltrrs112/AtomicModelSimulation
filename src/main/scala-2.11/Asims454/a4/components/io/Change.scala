package Asims454.a4.components.io

import Asims454.a2.{Nickel, Quarter, Dime, NoCoin, Coin}
import raxsim.io.{Token, Output}

/**
 */
case class Change(change : collection.immutable.IndexedSeq[Coin]) extends Output[Int] with Token {
  /**
   * Adds two changes together.
   * @param other
   *              Another change object
   * @return
   *         A changed object equal to the sum of this and other.
   */
  def ++ (other : Change): Change = {
    val totalChange : collection.immutable.IndexedSeq[Coin] = change ++ other.change
    Change(totalChange)
  }

  /**
   * @param coin
   *             Adds the coin to the current bag of change and returns it
   */
  def + (coin : Coin): Change ={
    val totalChange : scala.collection.mutable.Buffer[Coin] = (change.toBuffer += coin).filter(_.value != 0)
    Change(totalChange.toIndexedSeq)
  }

  /**
   * The total value of all the coins in the bag.
   * @return
   *         The value of all the coins
   */
  def value = change.foldLeft(0)(_ + _.value)

  override def toString : String = {
    var quarter = 0
    var nickels = 0
    var dimes = 0
    change.foreach {
      case Quarter() => quarter += 1
      case Nickel() => nickels += 1
      case Dime() => dimes += 1
      case _ => Unit
    }

    "Q : " + quarter + ", N : " + nickels + ", D: " + dimes
  }

}

object ChangeBuilder {
  def apply(coins : Int*): Change = {
    val coinSequence : Seq[Coin] = for(value <- coins if Seq(5,10,25,0).contains(value) ) yield {
      value match {
        case 5 => Nickel()
        case 10 => Dime()
        case 25 => Quarter()
        case 0 => NoCoin()
      }
    }
    Change(collection.immutable.IndexedSeq(coinSequence : _*))
  }
}
object test extends App {
  println(ChangeBuilder(25,25,5,5,5))
}