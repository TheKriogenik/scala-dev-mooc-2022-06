package homework

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class OptTest extends AnyFlatSpec {

  import homework.opt._

  it should "zip two existing values" in {
    val x1 = Option(1)
    val x2 = Option("abc")
    assert(x1.zip(x2) == Option(1, "abc"))
  }

  it should "not zip when one value not exists" in {
    val exists = Option.Some(1)
    val notExists = Option.None
    assert(exists.zip(notExists) == Option.None)
    assert(exists.zip(notExists) == notExists.zip(exists))
  }

  it should "not zip when both is empty" in {
    assert(Option.None.zip(Option.None) == Option.None)
  }

  it should "print if exists" in {
    val stream = new ByteArrayOutputStream()
    val msg = "test"
    val actual = Console.withOut(stream) {
      Option(msg).printIfAny()
      stream.toString
    }
    assert(actual == msg + "\r\n")
  }

  it should "not print if empty" in {
    val stream = new ByteArrayOutputStream()
    val actual = Console.withOut(stream) {
      Option.None.printIfAny()
      stream.toString()
    }
    assert(actual.isEmpty)
  }

  it should "filter if exists" in {
    val f: Int => Boolean = x => x%2==0

    assert(Option(2).filter(f) == Option(2))
    assert(Option(1).filter(f) == Option.None)

  }


}
