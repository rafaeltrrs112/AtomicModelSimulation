package raxsimulate.io

/**
 *
 */
trait Output[T] extends Token {
  val value: T
}
//Used by routers to order their outputs.
case class WrappedToken(value : Seq[Token]) extends Output[Seq[Token]]
