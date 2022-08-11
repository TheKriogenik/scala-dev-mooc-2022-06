package homework

import org.scalatest.flatspec.AnyFlatSpec

class ListTest extends AnyFlatSpec {

  import homework.list._

  it should "correct print list" in {
    val xs = cons(1, cons(2, cons(3, List.Nil)))
    val expected = "[1, 2, 3]"
    val actual = mkString(xs, ",")
    assert(expected == actual)
  }

  it should "correct build from varargs" in {
    val actual = List(1, 2, 3, 4, 5)
    val expected = 1 :: 2 :: 3 :: 4 :: 5 :: List.Nil
    assert(actual == expected)
  }

  it should "correct reverse" in {
    val actual = List(1, 2, 3, 4, 5)
    val expected = List(5, 4, 3, 2, 1)
    assert(reverse(actual) == expected)
  }

  it should "correct map" in {
    val actual = List(1, 2, 3, 4, 5)
    val expected = List(1, 4, 9, 16, 25)
    val f: Int => Int = x => x*x
    assert(map(actual, f) == expected)
  }

  it should "correct filter" in {
    val actual = List(1, 2, 3, 4, 5)
    val expected = List(2, 4)
    val f: Int => Boolean = x => x % 2 == 0
    assert(filter(actual, f) == expected)
  }

}
