// exercise 4.6 : implement versions of map, flatMap, orElse, map2
// that operate on the Right side

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B]
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B]
  def orElse[EE >: E, AA >: A](b: => Either[EE,AA]): Either[EE, AA]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E, +A](e: E) extends Either[E, A] {
  override def map[B](f: A => B): Either[E,B] = Left(e)
  override def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = Left(e)
  override def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = Left(e)
}

case class Right[+E, +A](a : A) extends Either[E, A] {
  override def map[B](f: A => B): Either[E,B] = Right(f(a))
  override def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = f(a)
  override def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = Right(a)
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b.map(f(a, _))
}


// exercise 4.7 : implement sequence and traverse for Either. These should
// return the first error that's encountered, if there is one
def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
  es match {
    case Nil => Right(Nil)
    case x :: xs => x.map2(sequence(xs)) { _ :: _ }
  }

def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
  as match {
    case Nil => Right(Nil)
    case x :: xs => f(x).map2(traverse(xs)(f)) { _ :: _ }
  }


def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
  es.foldRight( Right(Nil) : Either[E,List[A]] ) { (a,l) => a.map2(l)(_ :: _) }