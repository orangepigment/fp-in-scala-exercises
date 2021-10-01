package ru.orangepigment.exercises.chapter5

import ru.orangepigment.exercises.chapter5.Stream.cons

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() +: t().toList
    }

  def take(n: Int): Stream[A] =
    if (n <= 0) {
      Stream.empty[A]
    } else {
      this match {
        case Empty => Empty
        case Cons(h, t) => cons(h(), t().take(n - 1))
      }
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) {
      this
    } else {
      this match {
        case Empty => Empty
        case Cons(_, t) => t().drop(n - 1)
      }
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) {
          cons(h(), t().takeWhile(p))
        } else {
          Empty
        }
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (p(a)) cons(a, acc) else Stream.empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[AA >: A](that: => Stream[AA]): Stream[AA] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
