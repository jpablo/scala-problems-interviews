package puzzles.leetCode.medium

import scala.annotation.tailrec


@tailrec
def fastPowTR2(x: Double, n: Long, i: Long = 0, acc: Double = 1, acc1: Double = 1): Double = {
  if (i >= n)
    acc * acc1
  else if (i  == 0)
    fastPowTR2(x, n, 1, x, acc1)
  else if (i <= n / 2.0)
    fastPowTR2(x, n, i * 2, acc * acc, acc1)
  else
    fastPowTR2(x, n - i, 1, x, acc * acc1)
}


@tailrec
def slowPowTR(x: Double, nTot: Int, nLeft: Int, acc: Double): Double = {
  if (nLeft <= 0)
    acc
  else {
    val next = acc * x
    slowPowTR(x, nTot, nLeft - 1, next)
  }
}


def fastPow(x: Double, n: Int): Double = {
  if (n == 0)
    1
  else {
    val isEven = n % 2 == 0
    val half = fastPow(x, n / 2)
    val mid = if (isEven) 1 else x
    half * mid * half
  }
}

def myPow(x: Double, n: Int): Double =
  if (n == 0)
    1
  else
    fastPowTR2(
      if (n < 0) 1 / x else x, 
      if (n < 0) -(n.toLong) else n
    )


@main def mainPow =
  // println(myPow(2.0, -2147483648))
  // println(myPow(0.00001, 2147483647))
  // println(myPow(1.72777, 7))
  // println(myPow(2.0, -2))
  println(myPow(2.0, 0))
  println(myPow(2.0, 1))
  println(myPow(2.0, 2))
  println(myPow(2.0, 3))
  println(myPow(2.0, 4))
  println(myPow(2.0, 5))
  println(myPow(2.0, 6))
  // println(myPow(2.0, 20))
  // println(myPow(1.0, 30))
  // println(myPow(1.0, 1483_647))
  // println(myPow(1.0, 2_147_483_647))