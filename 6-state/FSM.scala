sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  def step(machine: Machine, input: Input) = (machine, input) match {
    case (Machine(_, 0, _), _) => machine
    case (Machine(true, _, _), Turn) => machine
    case (Machine(false, _, _), Coin) => machine
    case (Machine(true, _, coins), Coin) => machine.copy(locked = false, coins = coins+1)
    case (Machine(false, candies, _), Turn) => machine.copy(locked = true, candies = candies-1)
  }

  State(machine => {
    val m1 = inputs.foldLeft(machine)(step)
    ((m1.coins, m1.candies), m1)
  })
}
