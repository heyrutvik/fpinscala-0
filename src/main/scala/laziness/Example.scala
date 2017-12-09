package laziness

object Example {

  /**
    * compiled to `public static <A> A if1(boolean, scala.Function0<A>, scala.Function0<A>)`
    */
  def if1[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse
}
