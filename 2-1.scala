import scala.annotation._

def fib(n: Int): Int = {
  @tailrec
  def f(a: Int, b: Int, n: Int): Int =
    if (n > 1) f(b, a+b, n-1)
    else a

  f(0, 1, n)
}
