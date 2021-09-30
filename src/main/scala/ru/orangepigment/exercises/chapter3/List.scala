package ru.orangepigment.exercises.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, count) => count + 1)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((count, _) => count + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (reversed, x) =>
      Cons(x, reversed)
    }

  def apply[A](l: A*): List[A] =
    if (l.isEmpty) Nil
    else Cons(l.head, apply(l.tail: _*))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](l: List[A], value: A): List[A] =
    l match {
      case Nil => Cons(value, Nil)
      case Cons(_, tail) => Cons(value, tail)
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    } else {
      l
    }
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)(Cons(_, _))

  def concat[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, Nil: List[A])(appendViaFoldRight)

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
    }

  def incr(l: List[Int]): List[Int] = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) Cons(head, filter(tail)(f)) else filter(tail)(f)
    }

  def removeOdds(l: List[Int]): List[Int] = filter(l)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) { a =>
      if (f(a)) Cons(a, Nil) else Nil
    }

  def sumLists(l1: List[Int], l2: List[Int]): List[Int] =
    l1 match {
      case Nil => Nil
      case Cons(head1, tail1) =>
        l2 match {
          case Nil => Nil
          case Cons(head2, tail2) => Cons(head1 + head2, sumLists(tail1, tail2))
        }
    }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =
    l1 match {
      case Nil => Nil
      case Cons(head1, tail1) =>
        l2 match {
          case Nil => Nil
          case Cons(head2, tail2) => Cons(f(head1, head2), zipWith(tail1, tail2)(f))
        }
    }

  def contains[A](l: List[A], x: A): Boolean =
    foldLeft(l, false)((z, a) => z || a == x)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def impl(sup: List[A], sub: List[A])
            (origSup: List[A] = sup, origSub: List[A] = sub): Boolean = {
      sub match {
        case Nil => true
        case Cons(subHead, subTail) =>
          sup match {
            case Nil => false
            case Cons(supHead, supTail) =>
              if (subHead == supHead)
                impl(supTail, subTail)(origSup, origSub)
              else {
                origSup match {
                  case Nil => false
                  case Cons(_, origSupTail) => impl(origSupTail, origSub)()
                }
              }
          }
      }
    }

    impl(sup, sub)()
  }

}
