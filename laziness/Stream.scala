trait Stream[+A] {
  // exercise 5.1: Stream -> List
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // exercise 5.2: take(n)
  def take(n): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons(h(), t().take(n-1))
      case _ => Empty
    }

  // exercise 5.3: takeWhile(p: A => Boolean): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h) => Cons(h(), t().takeWhile(p))
      case _ => Empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // exercise 5.4: forAll(p: A => Boolean): Boolean
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Nil => true
      case Cons(h, t) if p(h()) => t().forAll(p)
      case _ => false
    }

  def forAll_1(p: A => Boolean): Boolean =
    foldRight(true) { (h,t) => p(h) && t }

  // exercise 5.5: Use foldRight to implement takeWhile
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) =>
      if (p(h)) cons(h, t)
      else empty
    }

  // exercise 5.6: Implement headOption using foldRight
  def headOption: Option[A] =
    foldRight(None: Option[A]) { (h,_) => Some(h) }

  // exercise 5.7: Implement map, filter, append (non-strict on its arg), flatMap using foldRight
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (h,t) => cons(f(h), t) }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h,t) =>
      if (p(h)) cons(h,t)
      else t
    }

  def append[B>:A]](a: => Stream[B]): Stream[B] =
    foldRight(a) { cons(_,_) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (h,t) =>
      f(h).append(t)
    }

  // exercise 5.13: Use unfold to implement map, take, takeWhile, zipWith and zipAll
  // zipAll should continue the traversal as long as either stream has more elements
  def map_1[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }

  def take_1(n: Int): Stream[A] =
    Stream.unfold((n,this)) {
      case (i,Cons(h,t)) if i > 0 => Some(h(), (i-1,t()))
      case _ => None
    }

  def takeWhile_1(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B](other: Stream[A])(f: (A,A) => B): Stream[B] =
    Stream.unfold((this,other)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this,other)) {
      case (Empty,Empty) => None
      case (Cons(h1,t1), Empty) => Some((Some(h1()),None), (t1(),Empty))
      case (Empty, Cons(h2,t2)) => Some((None,Some(h2())), (Empty,t2()))
      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(),t2()))
    }

  // exercise 5.14: Implement startsWith
  def startsWith[A](s: Stream[A]): Boolean =
    zipWith(s)(_ == _).forAll(_ == true)

  // exercise 5.15: Implement tails using unfold
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case s@Cons(h,t) => Some((s,t()))
      case _ => None
    } append Stream.empty

  // exercise 5.16: Generalize tails to scanRight
  def scanRight[B](z: B)(fn: (A, => B) => B): Stream[B] =
    foldRight(z -> Stream(z)) {
      case (a, t) =>
        lazy val (b1, s) = t
        lazy val b2 = fn(a,b1)
        b2 -> cons(b2, s)
    }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  // exercise 5.8: Generalize `ones`
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // exercise 5.9: Write a function that generates an infinite stream of integers n, n+1, ...
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // exercise 5.10: Write a function fibs that generates an infinite stream of Fibonacci numbers:
  // 0,1,1,2,3,5,8,...
  def fibs: Stream[Int] = {
    def fn(a: Int, b: Int): Stream[Int] = cons(b, fn(b, a+b))
    fn(0,1)
  }

  // exercise 5.11: Write a more general stream building method called unfold
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case Some((a,state)) => cons(a, unfold(state)(f))
      case None => empty
    }

  // exercise 5.12: Write fibs, from, constant and ones in terms of unfold
  def ones_1: Stream[Int] = unfold(1) {s => Some( (s,s) )}
  def constant_1[A](a: A): Stream[A] = unfold(a) {s => Some( (s,s) )}
  def from_1(n: Int): Stream[Int] = unfold(n) {s => Some( (s,s+1) )}
  def fibs: Stream[Int] = unfold( (0,1) ) { s => Some( (s._1, (s._2, s._1+s._2)) )}
}
