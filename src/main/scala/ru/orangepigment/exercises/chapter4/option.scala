package ru.orangepigment.exercises.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(get) => Some(f(get))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(get) => get
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _ => this
    }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil)) { (o, acc) =>
      map2(o, acc) {
        _ +: _
      }
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case head :: tail => f(head).flatMap(h => traverse(tail)(f).map(h +: _))
      case Nil => Some(Nil)
    }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}

object OptionExercises {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  val absO: Option[Double] => Option[Double] = Option.lift(math.abs)

}