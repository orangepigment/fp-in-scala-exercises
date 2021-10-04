package ru.orangepigment.exercises.chapter10

import ru.orangepigment.exercises.chapter3.{Branch, Leaf, Tree}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)
}

object Foldable {

  object ListFoldable extends Foldable[List] {

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  object StreamFoldable extends Foldable[LazyList] {
    def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: LazyList[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.map(f(_, z)).getOrElse(z)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.map(f(z, _)).getOrElse(z)

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).getOrElse(mb.zero)
  }


}