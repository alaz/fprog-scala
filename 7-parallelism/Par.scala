import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // exercise 7.3: fix the implementation of map2 so that it respect the contract of timeouts on Future
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(f(af.get, bf.get))
    }

  private case class Map2Future[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C] {
    private var cell = Option.empty[C]

    override def isDone = cell.isDefined
    override def get(timeout: Long, units: TimeUnit): C =
      if (isDone) cell.get
      else {
        val ms = TimeUnit.MILLISECONDS.convert(timeout, units)
        val started = System.currentTimeMillis
        val a = af.get(ms, TimeUnit.MILLISECONDS)
        val elapsed = System.currentTimeMillis-started
        val b = bf.get(ms-elapsed, TimeUnit.MILLISECONDS)
        val c = f(a, b)
        cell = Some(c)
        c
      }
    override def get: C = get(Long.MaxValue, TimeUnit.MILLISECONDS)
    override def isCancelled = af.isCancelled || bf.isCancelled
    override def cancel(evenIfRunning: Boolean): Boolean =
      if (cell.isDefined) false
      else af.cancel(evenIfRunning) && bf.cancel(evenIfRunning)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // exercise 7.4: using lazyUnit, write a function to convert any function A => B to one that evaluates
  // its result asynchronously
  def asyncF[A,B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.reverse.foldLeft(unit(List.empty[A])) { (pl, pa) =>
      map2(pa, pl) { _ :: _ }
    }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val aToList: A => Par[List[A]] = asyncF { Some(_).filter(f).toList }
    map(sequence(as map aToList))(_.flatten)
  }

  // exercise: is there a more general version of parallel `sum`?
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r))) { _ + _ }
    }

  def reduce[A](is: IndexedSeq[A])(f: (A,A) => A): Par[A] = {
    require(is.nonEmpty)
    is match {
      case Seq(x) =>
        Par.unit(x)
      case _ =>
        val (l,r) = is.splitAt(is.length/2)
        Par.map2(Par.fork(reduce(l)(f)), Par.fork(reduce(r))(f))(f)
    }
  }

  def sum_1(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.isEmpty) Par.unit(0)
    else reduce(ints) { _ + _ }

  // exercise: paragrahs -> number of words
  def numWords(l: List[String]): Par[Int] = {
    val words = map(Par.lazyUnit(l)) { _.split("""[ .,]+""") }
    map(words) { _.length }
  }

  // exercise: map3, map4, etc.
  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] =
    map2(map2(pa,pb)(identity), pc) { (ab, c) => f(ab._1, ab._2, c) }

  // Exercise 7.11: implement choiceN, then choice via choiceN
  // Exercise 7.12: choiceMap
}
