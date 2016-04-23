package state

import state.State

object machine {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {

//    def simulateMachine(inputs:List[Input]): State[Machine, (Int, Int)] = {
//      def foo(input: Input): State[Machine, (Int, Int)] = {
//          State(s => {
//            input match {
//              case Coin if s.candies > 0 => ((s.candies, s.coins + 1), s.copy(locked = false))
//              case Turn if !s.locked => ((s.candies - 1, s.coins), s.copy(locked = true))
//              case _ => ((s.candies, s.coins), s)
//            }
//          })
//      }
//      inputs.foldLeft(State.unit())
//    }
  }
}
