package a3

import raxsimulate.io.{Token, WrappedToken}
import raxsimulate.model.Model

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class XORToken(value: Boolean) extends Token

object XORToken {
  def apply(b: Boolean*): IndexedSeq[XORToken] = b.map(XORToken(_)).toIndexedSeq
}

/**
 *
 * @param modelName
 */
class XORModel(modelName: String) extends Model() {
  override val name = modelName

  var _currentOutput: Option[IndexedSeq[Token]] = Some(ArrayBuffer[XORToken]())
  var _currentState: mutable.Map[String, String] = mutable.Map[String, String]()

  /**
   *
   * @param input
   * List of XOR tokens to process. True only if odd number of inputs are true
   *
   */
  override def stateTransition(input: IndexedSeq[Token]): Unit = {
    var result: mutable.ArrayBuffer[XORToken] = mutable.ArrayBuffer[XORToken]()

    input.foreach { (token) => {
      token match {
        case WrappedToken(inputs) => inputs.foreach { (innerToken) => {
          innerToken match {
            case xor: XORToken =>
              result += xor
          }
        }
        }
        case xor: XORToken => result += xor
      }
    }
    }

    _currentOutput = Some(IndexedSeq[XORToken](result.fold(XORToken(false))(XOR)))
    _currentState("Output: ") = _currentOutput.toString
  }


  /*
   *
   */
  private def XOR(xOne: XORToken, xTwo: XORToken): XORToken = {
    if ((!xOne.value && xTwo.value) || (xOne.value && !xTwo.value)) XORToken(true) else XORToken(false)
  }

  override def toString = "XOR\n" +
    "CurrentState: " + _currentState

  /**
   *
   * @return
   */
  override def currentOutput: Option[IndexedSeq[Token]] = _currentOutput

  /**
   *
   * @return
   */
  override def currentState: mutable.Map[String, String] = _currentState
}
