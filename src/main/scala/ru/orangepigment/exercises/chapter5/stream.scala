package ru.orangepigment.exercises.chapter5

import ru.orangepigment.exercises.chapter5.Stream.{cons, empty, unfold}

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
      Empty
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
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[AA >: A](that: => Stream[AA]): Stream[AA] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Option(f(h()), t())
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) { case (a, p0) =>
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }._2

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


  lazy val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = cons(a, stream)
    stream
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((value, state)) => cons(value, unfold(state)(f))
      case None => empty
    }
  }

  val fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (f0, f1) =>
    Some((f0, (f1, f0 + f1)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n) { a => Some((a, a + 1)) }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a) { a => Some((a, a)) }

  val onesViaUnfold: Stream[Int] = unfold(1) { _ => Some((1, 1)) }

}
