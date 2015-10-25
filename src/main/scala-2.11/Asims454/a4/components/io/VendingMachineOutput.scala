package Asims454.a4.components.io

import raxsim.io.Token
import scala.collection.immutable.IndexedSeq

/**
 */
case class VendingMachineOutput(coffee : IndexedSeq[Coffee], change : Option[Change]) extends Token