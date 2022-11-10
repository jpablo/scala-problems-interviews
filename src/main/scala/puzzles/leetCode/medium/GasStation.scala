package puzzles.leetCode.medium

// # 134. Gas Station

// Time: O(n)
// Space: O(1)
def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
  val n = gas.length
  var totalTank = 0
  var currTank = 0
  var start = 0
  for (i <- 0 until n) {
    val toNextStation = gas(i) - cost(i)
    totalTank += toNextStation
    currTank += toNextStation
    println((i, toNextStation, totalTank, currTank))
    if (currTank < 0) {
      start = i + 1
      println(s"start: $start")
      currTank = 0
    }
  }
  if (totalTank >= 0) start else -1
}




@main def mainGS =
  val gas  = Array(1,2,3,4,5)
  val cost = Array(3,4,5,1,2)

  // val nextMove = gas.zip(cost).map(_ - _)
  // println(nextMove.toList)

  val c = canCompleteCircuit(gas, cost)
  println(c)
