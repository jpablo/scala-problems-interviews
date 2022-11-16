package puzzles.leetCode.easyProblems


/* 

Given a non-negative integer x, return the square root of x rounded down to the nearest integer. The returned integer should be non-negative as well.

You must not use any built-in exponent function or operator.

For example, do not use pow(x, 0.5) in c++ or x ** 0.5 in python.
 */



/* 
Ideas:
- binary search
- math fact: 
  if x >= 2 then 0 < sqrt(x) < x / 2

Attention:
- Integer overflows
*/

sealed trait Ord
object Ord {
  case object LT extends Ord
  case object GT extends Ord
  case object EQ extends Ord

  def compare(a: Long, b: Long): Ord =
    if (a > b) 
      Ord.GT 
    else if (a < b) 
      Ord.LT 
    else 
      Ord.EQ      
}
import Ord._

def binSearch(left: Int, right: Int, p: Int => Ord): Int =
  if (left <= right) {
    val guess = left + (right - left) / 2 
    p(guess) match {
      case GT => binSearch(left     , guess - 1, p)
      case LT => binSearch(guess + 1, right    , p)
      case EQ => guess // exact answer
    }
  } else
    right // approximation

def mySqrt(x: Int): Int =
  if (x < 2)
    x
  else
    binSearch(2, x / 2, g => Ord.compare(g.toLong * g, x))

def mySqrt0(x: Int): Int = {
  if (x < 2)
    x
  else {
    var left = 2
    var right = x / 2
    while (left <= right) {
      val guess = left + (right - left) / 2
      val num: Long = guess.toLong * guess
      if (num > x)
        right = guess - 1
      else if (num < x)
        left = guess + 1
      else
        return guess
    }
    right
  }
}


@main def main69 =
  // assert(mySqrt(4) == 2)
  // assert(mySqrt(8) == 2)
  println(mySqrt(2147395599))
  // println(mySqrt0(2147395599))

  println("Ok")