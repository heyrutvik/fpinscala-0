package functionalstate

/**
  * State Action or Transition
  * @param run
  * @tparam S State
  * @tparam V Value
  */
case class Action[S, +V](run: S => (V, S)) {

  def map[A](f: V => A): Action[S, A] = new Action(state => {
    val (v, s1) = this.run(state)
    (f(v), s1)
  })

  def flatMap[A](f: V => Action[S, A]): Action[S, A] = new Action(state => {
    val (v, s1) = this.run(state)
    f(v).run(s1)
  })

  def map2[A, B](action: Action[S, A])(f: (V, A) => B): Action[S, B] = new Action(state => {
    val (v1, s1) = this.run(state)
    val (v2, s2) = action.run(s1)
    (f(v1, v2), s2)
  })
}

object Action {

  def unit[S, V](v: V): Action[S, V] = new Action(state => (v, state))

  def sequence[S, A](actions: List[Action[S, A]]): Action[S, List[A]] = {
    actions.foldRight(unit[S,List[A]](Nil))((action, z) => action.map2(z)(_ :: _))
  }
}