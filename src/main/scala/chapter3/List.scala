package chapter3

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
    ???
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(List.reverseViaFoldLeft(as), z)((b,a) => f(a,b))
  }

  def appendViaFold[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)((a, acc) => Cons(a, acc))

  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil:List[A])((acc, a) => appendViaFold(acc, a))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((h,t) => Cons(f(h),t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => appendViaFold(f(h), t))

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] =
    (a1, a2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}
