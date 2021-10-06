package ru.orangepigment.exercises.chapter12

import ru.orangepigment.exercises.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def applyViaMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((fa, a) => fa(a))

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(
      apply(unit(f.curried))(fa)
    )(fb)

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(
      apply(
        apply(unit(f.curried))(fa)
      )(fb)
    )(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(
      apply(
        apply(
          apply(unit(f.curried))(fa)
        )(fb)
      )(fc)
    )(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A])) { case (fa, acc) =>
      map2(fa, acc)(_ :: _)
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    List.fill(n)(fa).foldRight(unit(List.empty[A])) { case (fa, acc) =>
      map2(fa, acc)(_ :: _)
    }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => a -> b)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))

      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
      //self.map2(fab, fa)((gab, ga) => g.map2(gab, ga)((ab, a) => ab(a)))
        self.map2(fab, fa)(g.map2(_, _)(_ (_)))
    }
  }

}

object Applicative {
  object StreamApplicative extends Applicative[LazyList] {
    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a)

    override def apply[A, B](fab: LazyList[A => B])(fa: LazyList[A]): LazyList[B] =
      fab.zip(fa).map { case (ab, a) => ab(a) }
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {

      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        fab match {
          case Failure(head, tail) =>
            fa match {
              case Failure(head2, tail2) => Failure(head, tail ++ (head2 +: tail2))
              case Success(_) => Failure(head, tail)
            }
          case Success(ab) =>
            fa match {
              case Failure(head, tail) => Failure(head, tail)
              case Success(a) => Success(ab(a))
            }
        }
    }
}

trait MinimalMonad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object MinimalMonad {
  def eitherMonad[E]: MinimalMonad[({type f[x] = Either[E, x]})#f] =
    new MinimalMonad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def apply[A, B](fab: Either[E, A => B])(fa: Either[E, A]): Either[E, B] =
        fab match {
          case Left(e) => Left(e)
          case Right(ab) =>
            fa match {
              case Left(e) => Left(e)
              case Right(a) => Right(ab(a))
            }
        }
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]