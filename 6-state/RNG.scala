trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    val i1 =
      if (i < 0) -(i+1)
      else i
    (i1, rng1)
  }

  // helper for testing
  case class Const(i: Int) extends RNG {
    def nextInt: (Int, RNG) = (i, this)
  }

  // exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng1)
  }

  // exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i,d), rng2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d,i), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  // exercise 6.4

  // naïve implementation via standard `List` methods
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val l = List.iterate[(Int,RNG)](rng.nextInt, count) { state => state._2.nextInt }
    (l.map(_._1), l.last._2)
  }

  // a low-level recursive implementation
  def ints_alt(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec def fn(acc: List[Int], r: RNG, n: Int): (List[Int], RNG) =
      if (n == 0) (acc.reverse, r)
      else {
        val (x,r1) = r.nextInt
        fn(x :: acc, r1, n-1)
      }

    fn(Nil, rng, count)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // exercise 6.5
  val double_2: Rand[Double] = map(nonNegativeInt) {i => i.toDouble / Int.MaxValue}

  // exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit[List[A]](Nil)) { (rl, ra) => map2(ra, rl) { _ :: _ } }

  // helper for testing:
  case class It(it: Iterator[Int]) extends RNG {
    def nextInt = (it.next, this)
  }

  def ints_2(count: Int) = sequence[Int](List.fill(count)(int))

  // in REPL:
  // ints_2(10) (It(Iterator.from(0)))

  // exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  // exercise 6.9
  def map_2[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra) { a =>
      map_2(rb) { f(a,_) }
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6)) { _ + 1 }

  //type Rand[A] = State[RNG, A]
}
