package chapter3

import chapter3.List.foldLeft

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new IllegalArgumentException("")
    case Cons(h, t) => t
  }

  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil => List(newHead)
    case Cons(h, t) => Cons(newHead, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def go(as: List[A], r: Int): List[A] =
      if (r == 0 || as == Nil) as
      else go(List.tail(as), r - 1)

    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(aas: List[A], acc: B): B = aas match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }

    go(as, z)
  }

  def sumViaFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productViaFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1.0d)(_ * _)

  def reverseViaFoldLeft[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((acc, a) => Cons(a, acc))

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    
  }
}
