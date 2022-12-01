package puzzles

import scala.annotation.tailrec

def whichIsBetter(maxNumber: (Int, Int), number: (Int, Int)): (Int, Int) =
  val n = number._1.toString
  val m = maxNumber._1.toString
  if maxNumber._1 == -1 then 
    number
  else if (n + m).toInt >= (m + n).toInt then
    number
  else
    maxNumber


@tailrec
def largestConcatenate(numbers: Vector[(Int, Int)], yourSalary: String = ""): String =
  if numbers.isEmpty then 
    yourSalary
  else
    val (maxNumber, maxIndex) = 
      numbers.foldLeft((-1, -1))(whichIsBetter)
    largestConcatenate(numbers.filterNot((_, i) => i == maxIndex), yourSalary + maxNumber)


def testCases = List(
  Vector(21, 2)         -> "221",
  Vector(9, 4, 6, 1, 9) -> "99641",
  Vector(23, 39, 92)    -> "923923",
)

@main def runMaximumSalary() =
  for (input, expected) <- testCases do
    val result = largestConcatenate(input.zipWithIndex)
    assert(expected == result, s"expected: $expected; found: $result")

