sealed trait Option[+A] {
  // exercise 4.1: implement the following
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): B
  def filter(f: A => Boolean): Option[A]
}
case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](ob: => Option[B]): B = this
  override def filter(f: A => Boolean): Option[A] =
    if (f(get)) this else None
}
case object None extends Option[Nothing] {
  override def map[B](f: A => B): Option[B] = None
  override def flatMap[B](f: A => Option[B]): Option[B] = None
  override def getOrElse[B >: A](default: => B): B = default
  override def orElse[B >: A](ob: => Option[B]): B = ob
  override def filter(f: A => Boolean): Option[A] = None
}

// exercise 4.2: implement variance in terms of flatMap
def variance(xs: Seq[Double]): Option[Double]

// exercise 4.3: write a generic function map2 that combines
// two Option values using a binary function
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  a flatMap { x =>
    b flatMap { y =>
      f(x,y)
    }
  }

def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  for { x <- a; y <- b } yield f(x,y)

// exercise 4.4: sequence
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(Nil)
    case x :: xs =>
      x flatMap { v =>
        sequence(xs) map { v :: _ }
      }
  }

// exercise 4.5: traverse
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(Nil)
    case x :: xs =>
      f(x) flatMap { v =>
        traverse(xs)(f) map { v :: _ }
      }
  }

def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
  traverse[Option[A],A](a)(x => x)
