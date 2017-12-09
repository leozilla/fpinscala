package chapter2

import org.scalatest._

class FibTests extends FlatSpec with Matchers {

  "Fib" should "be correct" in {
    Chapter2.fib(0) should be (0)
    Chapter2.fib(1) should be (1)
    Chapter2.fib(2) should be (1)
    Chapter2.fib(3) should be (2)
    Chapter2.fib(4) should be (3)
    Chapter2.fib(5) should be (5)
    Chapter2.fib(6) should be (8)
    Chapter2.fib(7) should be (13)
  }
}
