package one.lab.tasks.week.one

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Recursion._

class RecursionTest extends AnyFunSuite with Matchers {

  test("gcd should return valid responses") {
    gcd(222, 111) shouldBe 111
    gcd(2442, 2214) shouldBe 6
    gcd(9090, 60) shouldBe 30
    gcd(9090, -60) shouldBe 30
    gcd(33, 11) shouldBe 11
  }

  test("nthFibonacciNumber should return valid responses") {
    nthFibonacciNumber(-20) shouldBe 0
    nthFibonacciNumber(0) shouldBe 0
    nthFibonacciNumber(1) shouldBe 1
    nthFibonacciNumber(2) shouldBe 1
    nthFibonacciNumber(3) shouldBe 2
    nthFibonacciNumber(10) shouldBe 55
    nthFibonacciNumber(13) shouldBe 233
    nthFibonacciNumber(30) shouldBe 832040
  }

  test("tailRecursiveFibonacciNumber should return valid responses") {
    tailRecursiveFibonacciNumber(-20) shouldBe 0
    tailRecursiveFibonacciNumber(0) shouldBe 0
    tailRecursiveFibonacciNumber(1) shouldBe 1
    tailRecursiveFibonacciNumber(2) shouldBe 1
    tailRecursiveFibonacciNumber(3) shouldBe 2
    tailRecursiveFibonacciNumber(10) shouldBe 55
    tailRecursiveFibonacciNumber(13) shouldBe 233
    tailRecursiveFibonacciNumber(30) shouldBe 832040
  }
}
