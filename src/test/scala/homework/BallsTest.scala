package homework

import org.scalatest.flatspec.AnyFlatSpec

class BallsTest extends AnyFlatSpec {

  import homework.Balls._

  it should "simulate P(AB)" in {
    val e1 = 0.015
    val target: Double = (3/6.0) * (3/5.0)
    val times = 1_000_000
    assert(Math.abs(`simulate_P(AB)`(times) - target) < e1)
  }

  it should "simulate P(B|A)" in {
    val e1 = 0.05
    val target: Double = 3/5.0
    val times = 1_000_000
    assert(Math.abs(`simulate_P(B|A)`(times) - target) < e1)
  }

}
