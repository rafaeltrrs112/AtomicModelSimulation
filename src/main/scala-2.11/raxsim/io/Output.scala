package raxsim.io

/**
 *
 */
//Used by routers to order their outputs.
case class WrappedToken[T <: Token](value : IndexedSeq[T]) extends Output[IndexedSeq[T]]
