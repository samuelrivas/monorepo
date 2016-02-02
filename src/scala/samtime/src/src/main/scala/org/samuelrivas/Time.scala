package org.samuelrivas.samtime

object Time {

  /* TODO make nanosecs type safe (i.e. not a plain Long) */
  def systemTime:Long = System.nanoTime()

  /* TODO make this functional and pretty (i.e. offload the side effects and
   * thread functionally
   */
  /**
    *  Evaluate `f` and return the result along with the elapsed time in nanoseconds
    *  @param f the function to evaluate
    *  @param getTime the function to get the current system time in nanoseconds
    */
  def elapsed[A](f: => A, getTime: => Long = systemTime): (A, Long) = {
    val startTime = getTime
    val r = f
    val endTime = getTime
    (r, (endTime - startTime))
  }
}
