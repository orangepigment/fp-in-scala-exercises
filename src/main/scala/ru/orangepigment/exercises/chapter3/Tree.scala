package ru.orangepigment.exercises.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(value) => value
      case Branch(left, right) => math.max(max(left), max(right))
    }
  }

  def depth(t: Tree[Int]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + math.max(depth(left), depth(right))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maxViaFold(t: Tree[Int]): Int = fold(t)(a => a)(math.max)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + math.max(_, _))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
