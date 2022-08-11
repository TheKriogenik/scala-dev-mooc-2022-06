package homework

import scala.annotation.tailrec

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  sealed trait List[+T] {

    def ::[TT >: T](elem: TT): List[TT] = List.::(elem, this)

  }

  object List {

    case class ::[A](head: A, tail: List[A]) extends List[A]

    case object Nil extends List[Nothing]

  }

  /**
   * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
   *
   */

  def cons[T](head: T, xs: List[T]): List[T] = xs.::(head)


  /**
   * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
   *
   */

  def mkString[T](xs: List[T], splitter: String): String = {
    @tailrec
    def f(xs: List[T], splitter: String, acc: String = "["): String = xs match {
      case List.::(head, List.::(tail, List.Nil)) => s"$acc$head$splitter $tail]"
      case List.::(head, tail) => f(tail, splitter, s"$acc$head$splitter ")
      case List.Nil => acc ++ "]"
    }

    f(xs, splitter)
  }

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  def List[T](args: T*): List[T] = {
    @tailrec
    def f[A](xs: Seq[A], acc: List[A] = List.Nil): List[A] = xs.size match {
      case 0 => acc
      case _ => f(xs.tail, cons(xs.head, acc))
    }
    reverse(f(args))
  }

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  def reverse[T](lst: List[T]): List[T] = {
    @tailrec
    def f[A](xs: List[T], acc: List[T] = List.Nil): List[T] = xs match {
      case List.::(head, tail) => f(tail, head :: acc)
      case List.Nil => acc
    }
    f(lst)
  }

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */

  def map[A, B](lst: List[A], fun: A => B): List[B] = {
    @tailrec
    def f[T, Y](xs: List[T], g: T => Y, acc: List[Y] = List.Nil): List[Y] = xs match {
      case List.::(head, tail) => f(tail, g, cons(g(head), acc))
      case List.Nil => acc
    }
    reverse(f(lst, fun))
  }


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  def filter[A](lst: List[A], pred: A => Boolean): List[A] = {
    @tailrec
    def f[T](xs: List[T], p: T => Boolean, acc: List[T] = List.Nil): List[T] = xs match {
      case List.::(head, tail) if p(head) => f(tail, p, cons(head, acc))
      case List.::(_, tail)  => f(tail, p, acc)
      case List.Nil => acc
    }

    reverse(f(lst, pred))
  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */

  def incMap(lst: List[Int]): List[Int] = map[Int, Int](lst, _+1)

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

  def shoutString(lst: List[String]): List[String] = map[String, String](lst, x => "!" + x)

}
