package chapter2

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def go(c: Int, prev: Int, curr: Int): Int =
      if (c == 0) prev
      else go(c - 1, curr, prev + curr)

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
