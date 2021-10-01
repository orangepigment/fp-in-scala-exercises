package ru.orangepigment.exercises.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(value) => Left(value)
      case Right(value) => Right(f(value))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(_) => this
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      v <- this
      v2 <- b
    } yield f(v, v2)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case head :: tail => f(head).map2(traverse(tail)(f)) (_ +: _)
      case Nil => Right(Nil)
    }

}

object EitherExercises {

  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
}