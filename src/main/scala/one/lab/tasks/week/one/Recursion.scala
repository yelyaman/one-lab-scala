package one.lab.tasks.week.one

object Recursion {

  def printNTimes(n: Int, value: String): Unit =
    if (n >= 1) {
      println(value)
      printNTimes(n - 1, value)
    }

  def gcd(a: Long, b: Long): Long =
    if (a % b == 0) b
    else gcd(b, a % b)

  def nthFibonacciNumber(n: Int): Int =
    if (n < 1) 0
    else if (n == 1) 1
    else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    def innerFibo(prev: Int = 1, next: Int = 1, iter: Int = n): Int =
      iter match {
        case x if x < 1  => 0
        case x if x <= 2 => next
        case _           => innerFibo(next, next + prev, iter - 1)
      }
    innerFibo()
  }

}
