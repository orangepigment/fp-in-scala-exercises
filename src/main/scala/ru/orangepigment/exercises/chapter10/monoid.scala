package ru.orangepigment.exercises.chapter10

import ru.orangepigment.exercises.chapter8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    def zero: A => A = a => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    // Associativity
    Gen.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      Gen.forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length <= 1) {
      v.headOption.map(f).getOrElse(m.zero)
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // Only for ascending collections
  def isOrdered(is: IndexedSeq[Int]): Boolean = {
    val mon: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }

      val zero: Option[(Int, Int, Boolean)] = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(is, mon)(i => Some((i, i, true))).forall(_._3)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      a1 match {
        case Stub(chars1) =>
          a2 match {
            case Stub(chars2) => Stub(chars1 + chars2)
            case Part(lStub2, words2, rStub2) => Part(chars1 + lStub2, words2, rStub2)
          }
        case Part(lStub1, words1, rStub1) =>
          a2 match {
            case Stub(chars2) => Part(lStub1, words1, rStub1 + chars2)
            case Part(lStub2, words2, rStub2) =>
              if ((rStub1 + lStub2).nonEmpty) {
                Part(lStub1, words1 + words2 + 1, rStub2)
              } else {
                Part(lStub1, words1 + words2, rStub2)
              }
          }
      }
    }

    override def zero: WC = Part("", 0, "")
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(t1: (A, B), t2: (A, B)): (A, B) = (a.op(t1._1, t2._1), b.op(t1._2, t2._2))

    def zero: (A, B) = (a.zero, b.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map.empty[K, V]

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B = { a =>
      b.op(a1(a), a2(a))
    }

    def zero: A => B = _ => b.zero
  }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[Map[A, Int]] = new Monoid[Map[A, Int]] {
      def zero: Map[A, Int] = Map.empty[A, Int]

      def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, a.getOrElse(k, 0) + b.getOrElse(k, 9))
        }
    }

    as.foldLeft(m.zero) { case (acc, a) =>
      m.op(acc, Map(a -> 1))
    }
  } // Or simply foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

}

sealed trait WC // Word count

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

