package raxsimulate

import raxsimulate.io.Token

/**
 *
 */
trait Output[String] extends Token {
  val value: String
}
