package Asims454.a4.components.io

import Asims454.a2.Coin
import raxsim.io.Token
import raxsim.model.DiscreteTimeInput

/**
 */
class VendingMachineInput(seconds : Int, override val input : Coin) extends DiscreteTimeInput[Coin](seconds, input) with Token

object VendingMachineInput{
  def apply(seconds : Int, input : Coin) : VendingMachineInput  = new VendingMachineInput(seconds, input)
}
