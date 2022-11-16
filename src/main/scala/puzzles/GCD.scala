package puzzles

import scala.annotation.tailrec

def slowGCD (a: Int, b: Int) =
  @tailrec def loop(d: Int): Int =
    if d <= 1 then 1
    else if a % d == 0 && b % d == 0
    then d
    else loop(d - 1)
  loop(math.min(a, b))

@tailrec
def gcd2 (a: Int, b: Int, count: Int): Int =
  if a > 0 && b > 0 then
    if a >= b then gcd2(a - b, b, count + 1) else gcd2(a, b - a, count + 1)
  else
    println(s"count: $count")
    math.max(a, b)


@tailrec
def euclid (a: Int, b: Int, count: Int = 0): Int =
  if a <= 0 || b <= 0 then
    math.max(a, b)
  else
    euclid(b, a % b, count + 1)


@main def runEuclide =
  println {
    euclid(6, math.pow(10, 9).toInt, 0)
//    gcd2(math.pow(10, 9).toInt, 6, 0)
  }
