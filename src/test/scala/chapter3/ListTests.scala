package chapter3

import org.scalatest.{FlatSpec, Matchers}

class ListTests extends FlatSpec with Matchers {

  "tail of empty list" should "throw" in {
    assertThrows[IllegalArgumentException] {
      List.tail(Nil)
    }
  }

  "tail of list with one element" should "return Nil" in {
    List.tail(List(1)) should be (Nil)
  }

  "tail of list with at least one element" should "return tail" in {
    List.tail(List(1,2,3)) should be (List(2,3))
  }

  "setHead of empty list" should "return list with this element only" in {
    List.setHead(Nil, 1) should be (List(1))
  }

  "setHead of list with one element" should "return list with this element only" in {
    List.setHead(List(1), 2) should be (List(2))
  }

  "setHead of list with at least one element" should "return list with this element as head" in {
    List.setHead(List(1,2,3), 2) should be (List(2, 2,3))
  }

  "drop zero elements of empty list" should "return empty list" in {
    List.drop(Nil, 0) should be (Nil)
  }

  "drop one element of empty list" should "return empty list" in {
    List.drop(Nil, 1) should be (Nil)
  }

  "drop one element of none empty list" should "return this list without the dropped elements" in {
    List.drop(List(1,2,3), 1) should be (List(2,3))
  }

  "drop all elements of none empty list" should "return empty list" in {
    List.drop(List(1,2,3), 3) should be (Nil)
  }

  "dropWhile of empty list" should "return empty list" in {
    List.dropWhile(Nil, (a: Int) => a > 0) should be (Nil)
  }

  "dropWhile of list where head does not match" should "return same list" in {
    List.dropWhile(List(-1,2,3), (a: Int) => a > 0) should be (List(-1,2,3))
  }

  "dropWhile of list where only head matches" should "return tail of list" in {
    List.dropWhile(List(1,-2,-3), (a: Int) => a > 0) should be (List(-2,-3))
  }

  "dropWhile of list where multiple elements match" should "return remaining elements" in {
    List.dropWhile(List(1,2,-3), (a: Int) => a > 0) should be (List(-3))
  }

  "dropWhile of list where all elements match" should "return empty list" in {
    List.dropWhile(List(1,2,3), (a: Int) => a > 0) should be (List())
  }

  "init of empty list" should "throw" in {
    assertThrows[IllegalArgumentException] {
      List.init(List())
    }
  }

  "init of none empty list" should "return all but last element of list" in {
    List.init(List(1,2,3)) should be (List(1,2))
  }

  "length of empty list" should "return zero" in {
    List.length(List()) should be (0)
  }

  "length of list with one element" should "be 1" in {
    List.length(List(3)) should be (1)
  }

  "length of list with multiple elements" should "be correct" in {
    List.length(List(1,2,3)) should be (3)
  }

  "foldLeft of empty list" should "return zero element" in {
    List.foldLeft(List[Int](), 66)(_ + _) should be (66)
  }

  "foldLeft of list with one element" should "return element combined with zero element" in {
    List.foldLeft(List(1), 66)(_ + _) should be (67)
  }

  "foldLeft of list with multiple elements" should "return all elements combined with zero element" in {
    List.foldLeft(List(2,3,4), 1)(_ + _) should be (10)
  }

  "sumViaFoldLeft of empty list" should "return 0" in {
    List.sumViaFoldLeft(List()) should be (0)
  }

  "reverseViaFold of empty list" should "return empty list" in {
    List.reverseViaFoldLeft(List()) should be (List())
  }

  "reverseViaFold of list with one element" should "return list with this one element" in {
    List.reverseViaFoldLeft(List(1)) should be (List(1))
  }

  "reverseViaFold of list with multiple elements" should "return the reverse of that list" in {
    List.reverseViaFoldLeft(List(1,2,3)) should be (List(3,2,1))
  }

  "appendViaFold two empty list" should "return empty list" in {
    List.appendViaFold(List(), List()) should be (List())
  }

  "appendViaFold empty list to list" should "return new list" in {
    List.appendViaFold(List(1,2), List()) should be (List(1,2))
  }

  "appendViaFold list to empty list" should "return new list" in {
    List.appendViaFold(List(), List(1,2)) should be (List(1,2))
  }

  "appendViaFold two lists" should "return new list" in {
    List.appendViaFold(List(1,2), List(3,4)) should be (List(1,2,3,4))
  }

  "concat lists with one element" should "return single list" in {
    List.concat(List(List(1), List(2), List(3))) should be (List(1,2,3))
  }

  "concat empty list" should "return single list" in {
    List.concat(List(List(), List(1), List(2,3))) should be (List(1,2,3))
  }

  "flatMap" should "flatten" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "hasSubsequence of empty list" should "not have any subsequence" in {
    List.hasSubsequence(List(), List(1)) should be (false)
    List.hasSubsequence(List(), List(1,2)) should be (false)
  }
}
