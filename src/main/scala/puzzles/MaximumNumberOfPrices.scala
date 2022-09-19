package puzzles

def getPrices(n: Int, minValue: Int = 1): List[Int] =
  println((n, minValue))
  if n < 2 * minValue + 1 then List(n)
  else
    minValue :: getPrices(n - minValue, minValue + 1)


@main def runPrices =
  println {
    getPrices(6)
  }
