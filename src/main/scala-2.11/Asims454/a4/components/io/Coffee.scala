package Asims454.a4.components.io

import raxsim.io.Output

/**
 */
case class Coffee() extends Output[String] {
  override val value: String = "Starbucks \u00A9"
}