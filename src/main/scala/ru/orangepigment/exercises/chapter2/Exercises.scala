package ru.orangepigment.exercises.chapter2

import scala.annotation.tailrec

object Exercises {

  def fib(n: Int): Int = {
    @tailrec
    def impl(n: Int, current: Int = 1, prevPrev: Int = 0, prev: Int = 1): Int = {
      if (n == 1) {
        0
      } else if (n == 2) {
        1
      } else if (n == current) {
        prevPrev + prev
      } else {
        impl(n, current + 1, prev, prevPrev + prev)
      }
    }

    impl(n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def impl(n: Int = 0): Boolean = {
      if (n > as.length - 2) {
        true
      } else if (ordered(as(n), as(n + 1))) {
        impl(n + 1)
      } else {
        false
      }
    }

    impl()
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
