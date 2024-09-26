package homework

object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Covariant - animal родитель dog, Option[Animal] родитель Option[Dog]
  // Contravariant - animal родитель dog, Option[Dog] родитель Option[Animal]
  // Invariant - нет отношений

  // Вопрос вариантности

  sealed trait Option[+T] {


    def isEmpty: Boolean = this match {
      case Option.None => true
      case Option.Some(v) => false
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("get on empty Option")
    }

    def map[B](f: T => B): Option[B] =
      flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.None => Option.None
      case Option.Some(v) => f(v)
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny(): Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None => ()
    }


    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

    def zip[E](that: Option[E]): Option[(T, E)] = (this, that) match {
      case (Option.Some(v1), Option.Some(v2)) => Option.Some(v1, v2)
      case _ => Option.None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(f: T => Boolean): Option[T] = this match {
      case x@Option.Some(v) if f(v) => x
      case _ => Option.None
    }

  }

  object Option {

    final case class Some[T](v: T) extends Option[T]

    final case object None extends Option[Nothing]

    def apply[T](v: T): Option[T] = Some(v)
  }


}
