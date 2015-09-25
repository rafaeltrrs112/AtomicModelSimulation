/**
 */
trait Ob{
  var thing : Int
}
class money(_thing : Int) extends Ob{
  override var thing = _thing
}
object TestApp extends App{
  val a = new money(12)
  val thing : Seq[Ob] = scala.collection.mutable.ArrayBuffer[Ob](a)
}
