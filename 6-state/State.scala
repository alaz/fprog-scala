//type State[S, +A] = S => (A,S)

// exercise 6.10
case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
  def map[B](f: A => B): State[S, B] = flatMap { a =>
    State.unit(f(a))
  }
  def map2[B,C](state2: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap { a =>
      state2.map { f(a,_) }
    }
}

object State {
  def unit[S,A](a: A): State[S,A] = new State[S,A](s => (a,s))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldLeft(unit[S,List[A]](Nil)) { (rl, ra) => ra.map2(rl) { _ :: _ } }

  def get[S]: State[S,S] = State[S,S](s => (s,s))
  def set[S](s: S): State[S,Unit] = State(_ => ((),s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
