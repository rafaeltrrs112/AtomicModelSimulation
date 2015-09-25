import scala.collection.mutable.ArrayBuffer

/**
 *
 */
class State[A](var stateMap : scala.collection.mutable.Map[String, ArrayBuffer[A]])
