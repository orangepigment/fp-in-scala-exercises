package ru.orangepigment.exercises.chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

object Machine {

  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (i => (update andThen State.modify[Machine]) (i)))
    s <- State.get
  } yield (s.coins, s.candies)

}

case class Machine(locked: Boolean, candies: Int, coins: Int)
