sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  // exercise 3.2
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => sys.error("empty list")
      case Cons(_, t) => t
    }

  // exercise 3.3
  def setHead[A](list: List[A], h: A): List[A] =
    list match {
      case Nil => sys.error("empty list")
      case Cons(_, t) => Cons(h, t)
    }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    list match {
      case _ if n <= 0 => l
      case Nil => sys.error("empty list")
      case Cons(h, t) => drop(t, n-1)
    }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    list match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else list
    }

  // exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // exercise 3.7
  // NO. see chapter 5.


  // exercise 3.8

  // exercise 3.9
  def length[A](as: List[A]): Int =
    as.foldRight(0) { (_, n) => n+1 }

  // experiment, not an exercise
  def foldRight1[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    @annotation.tailrec
    def fn(acc: B => B, l: List[A]): B => B =
      l match {
        case Nil => acc
        case Cons(x, xs) => fn(f.curried(x) andThen acc, xs)
      }
    fn(x => x, as)(z)
  }

  // exercise 3.10 (tailrec foldLeft)
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
    }

  // exercise 3.11
  // sum, product, length via foldLeft
  def sum(as: List[Int]): Int = foldLeft(as, 0) { _ + _ }
  def product(as: List[Double]): Int = foldLeft(as, 1.0) { _ * _ }
  def length[A](as: List[A]): Int = foldLeft(as, 0) { _ + 1 }

  // exercise 3.12
  // reverse
  def reverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def f(l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(x, xs) => f(xs, Cons(x, acc))
      }
    f(as, Nil)
  }

  // via fold?
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A]) { (z,a) => Cons(a,z) }

  // exercise 3.13
  // foldLeft via foldRight
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    foldRight(as, (b: B) => b)( (a,g) => b => g(f(b,a)) ) (z)

  // other way around
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(as, (b: B) => b)( (g,a) => b => g(f(a,b)) ) (z)

  // exercise 3.14
  // append in terms of either foldLeft or foldRight
  def append[A](xs: List[A], x: A): List[A] =
    foldRight(xs, Cons(x, Nil)) { Cons(_, _) }

  def append1[A](xs1: List[A], xs2: List[A]): List[A] =
    foldRight(xs1, xs2) { Cons(_,_) }

  // exercise 3.15
  // concatenate list of lists into single list
  def concat[A](xs: List[List[A]]): List[A] =
    foldLeft(xs, Nil:List[A]) { foldRight(_, _) { Cons(_, _) } }

  // exercise 3.16 : transform a list of integers +1
  def listP1(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, listP1(xs))
    }

  // exercise 3.17 : transform a list of Double, turn every into string
  def listDtoS(l: List[Double]): List[String] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, listDtoS(xs))
    }

  // exercise 3.18 : map
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]) { (a,b) => Cons(f(a), b) }

  def map1[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

  // exercise 3.19 : filter
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)
    }

  // exercise 3.20 : flatMap
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => append1(f(x), flatMap(xs)(f))
    }

  // exercise 3.21 : filter via flatMap
  def filter1[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) { x => if (f(x)) Cons(x, Nil) else Nil }

  // exercise 3.22 : List[Int], List[Int] , sum elements (zip)

  // exercise 3.23 : zipWith
  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A,A) => B): List[B] =
    (l1, l2) match {
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1,x2), zipWith(xs1,xs2))
      case (Nil, Nil) => Nil
      case _ => sys.error("different list sizes")
    }

  // exercise 3.24 : hasSubsequence, functional and effective
  def hasSubsequence[A](full: List[A], sub: List[A]): Boolean =
    (full, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x1, f1), Cons(x2, f2)) =>
        if (x1 == x2) hasSubsequence(f1, f2)
        else hasSubsequence(f1, sub)
    }
}