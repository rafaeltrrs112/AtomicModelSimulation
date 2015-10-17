package raxsim.io

/**
 *
 */
trait Output[T] extends Token {
  val value: T
}
//Used by routers to order their outputs.
case class WrappedToken(value : IndexedSeq[Token]) extends Output[IndexedSeq[Token]]
