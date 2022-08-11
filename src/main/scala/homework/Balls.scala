package homework

import scala.util.Random

object Balls {

  sealed trait Ball

  case object Black extends Ball

  case object White extends Ball

  type Basket = List[Ball]

  def generateBasket(black: Int, white: Int): Basket = {
    val blackBalls = (0 to black).map(_ => Black).toList
    val whiteBalls = (0 to white).map(_ => White).toList
    Random.shuffle(blackBalls ++ whiteBalls)
  }

  val takeBalls: Basket => (Ball, Ball) = {
    case x :: y :: _ => (x, y)
  }

  def `simulate_P(AB)`(times: Int): Double = (1 to times)
    .map(_ => generateBasket(3, 3))
    .map(takeBalls(_))
    .filter {
      case (Black, White) => true
      case _ => false
    }.foldLeft(0)((a, _) => a + 1).toDouble / times

  def `simulate_P(B|A)`(times: Int): Double = List
    .fill(times)(generateBasket(2, 3))
    .filter{
      case White :: _ => true
      case _ => false
    }.foldLeft(0.0)((a, _) => a + 1) / times

}