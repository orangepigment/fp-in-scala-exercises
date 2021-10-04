package ru.orangepigment.exercises.chapter8

import ru.orangepigment.exercises.chapter6.{RNG, State}
import ru.orangepigment.exercises.chapter8.Gen._
import ru.orangepigment.exercises.chapter8.Prop._

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeLessThan(2)).map(_ == 1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       | ${e.getStackTrace.mkString("\n")}""".stripMargin

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => listOfN(n))

  def unsized: SGen[A] = SGen(_ => this)

}

case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]
  type MaxSize = Int
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(_, _) => p.run(max, n, rng)
        case x => x
      }
  }
}


object Exercises {

  val intList = Gen.listOf(Gen.choose(0, 100))
  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

}
