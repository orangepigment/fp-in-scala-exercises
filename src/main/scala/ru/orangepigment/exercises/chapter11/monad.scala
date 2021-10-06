package ru.orangepigment.exercises.chapter11

import ru.orangepigment.exercises.chapter6.State
import ru.orangepigment.exercises.chapter7.Par
import ru.orangepigment.exercises.chapter7.Par.Par
import ru.orangepigment.exercises.chapter8.Gen

// Monad laws:
//  x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
//  which is equivalent to compose(compose(f, g), h) == compose(f, compose(g, h))
//
//  compose(f, unit) == f
//  compose(unit, f) == f
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma match {
      case head :: tail => map2(head, sequence(tail))(_ :: _)
      case Nil => unit(Nil)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))((x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

}


object Monad {
  object GenMonad extends Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  object ParMonad extends Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  object OptionMonad extends Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  object ListMonad extends Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap(f)
  }

  object LazyListMonad extends Monad[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList(a)

    override def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
      ma.flatMap(f)
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State.unit(a)

    override def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B] =
      ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  object IdMonad extends Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)
  }

}

case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

}
