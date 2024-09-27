package module2

import scala.language.implicitConversions

object homework_hkt_impllicts {

  /**
   *
   * Доработать сигнатуру tupleF и реализовать его
   * По итогу должны быть возможны подобные вызовы
   * val r1 = println(tupleF(optA, optB))
   * val r2 = println(tupleF(list1, list2))
   *
   */
  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit ev1: BindableF[F, A], ev2: BindableF[F, B]): F[(A, B)] = {
    ev1(fa).flatMap(a => ev2(fb).map(b => (a, b)))
  }

  type BindableF[F[_], A] = F[A] => Bindable[F, A]

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]
  }

  implicit def bindableList[A](xs: List[A]): Bindable[List, A] = new Bindable[List, A] {
    override def map[B](f: A => B): List[B] = xs.map(f)

    override def flatMap[B](f: A => List[B]): List[B] = xs.flatMap(f)
  }

  implicit def bindableOption[A](xs: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = xs.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = xs.flatMap(f)
  }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))

  object second {
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

      def withFilter[A](x: F[A])(p: A => Boolean): F[A]
    }

    implicit class FunctorOps[F[_], A](x: F[A])(implicit t: Functor[F]) {
      def map[B](f: A => B): F[B] = t.map(x)(f)

      def flatMap[B](f: A => F[B]): F[B] = t.flatMap(x)(f)

      def withFilter(p: A => Boolean): F[A] = t.withFilter(x)(p)
    }

    object Functor {
      implicit val listFunctor: Functor[List] = new Functor[List] {
        override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

        override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

        override def withFilter[A](x: List[A])(p: A => Boolean): List[A] = x.filter(p)
      }

      implicit val optionFunctor: Functor[Option] = new Functor[Option] {
        override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

        override def withFilter[A](x: Option[A])(p: A => Boolean): Option[A] = x.filter(p)
      }

    }

    def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit ev: Functor[F]): F[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)

    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val r1 = println(tupleF(optA, optB))
    val r2 = println(tupleF(list1, list2))

  }

}