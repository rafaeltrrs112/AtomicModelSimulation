package Asims454.a4.components.event

/**
 *
 * @param ringing
 *                  Whether alarm is ringing or not.
 * @param duration
 *                 How long it has been ringing for.
 */
case class AlarmResponse(ringing : Boolean, duration : Int)
/**
 */
class Alarm(var triggerTime : Int) {
  val INFINITE : Int = -1
  triggerTime = INFINITE

  def ringing(implicit elapsedTime : Int) : Boolean = {
    if (triggerTime == INFINITE) false else if(elapsedTime >= triggerTime) true else false
  }

  def set(newTime : Int) : Unit = triggerTime = newTime
  def clear : Unit = triggerTime = INFINITE

  def ringingThenClear(implicit elapsedTime : Int) : Boolean = {
    if (triggerTime == INFINITE) false else if(elapsedTime >= triggerTime) {triggerTime = INFINITE ; true} else false
  }
}

object Alarm {
  def apply(triggerTime : Int) : Alarm = new Alarm(triggerTime)
}