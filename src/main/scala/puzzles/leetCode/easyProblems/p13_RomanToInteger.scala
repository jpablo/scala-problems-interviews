package puzzles.leetCode.easyProblems

// 13
// https://leetcode.com/problems/roman-to-integer/

// import org.scalameter.api.*

import puzzles.leetCode.easyProblems.RomanDigit.{C, D, I, L, M, V, X}

import scala.annotation.tailrec


// Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.

// Symbol       Value
// I             1
// V             5
// X             10
// L             50
// C             100
// D             500
// M             1000

enum RomanDigit:
  case I, V, X, L, C, D, M

  def toInt: Int = this match
    case I => 1
    case X => 10
    case C => 100
    case M => 1000
    case V => 5
    case L => 50
    case D => 500

// For example, 2 is written as II in Roman numeral, just two ones added together. 12 is written as XII,
// which is simply X + II. The number 27 = 20 + 5 + 1 + 1 is written as XXVII, which is XX + V + I + I.

// Roman numerals are usually written largest to smallest from left to right.

// However, the numeral for four is not IIII. Instead, the number four is written as IV.
// Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX.

// There are _six_ instances where subtraction is used:

// {1,2}. I (1)   can be placed before V (5) and X (10) to make 4 and 9.          // IV, IX
// {3,4}. X (10)  can be placed before L (50) and C (100) to make 40 and 90.      // XL, XC
// {5,6}. C (100) can be placed before D (500) and M (1000) to make 400 and 900.  // CD, CM

// Given a roman numeral, convert it to an integer.

def charToRomanDigit(s: Char): RomanDigit = s match
  case 'I' => I
  case 'V' => V
  case 'X' => X
  case 'L' => L
  case 'C' => C
  case 'D' => D
  case 'M' => M

val toInt =
  Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

type Index = Int

// Hints
// - Solve happy path first and then add exceptions
// - Process more than 1 element at a time
@tailrec
def romanToInt(roman: IndexedSeq[RomanDigit], i: Index = 0, acc: Int = 0): Int =
  if (i >= roman.length)
    acc
  else
    // current and next numbers
    val curr = roman(i).toInt
    lazy val next = roman(i + 1).toInt // lazy to avoid out of bounds
    // normal case: decreasing or equal
    if (i + 1 >= roman.length || curr >= next)
      // add current number
      romanToInt(roman, i + 1, acc + curr)
    else
      // exception: increasing
      // subtract current number to next one
      romanToInt(roman, i + 2, acc + next - curr)


def romanToIntFold(roman: String): Int =
  roman.foldLeft((0, 0): (acc: Int, prev: Int)):
    case ((acc, prev), currRoman) =>
      val curr = toInt(currRoman)
      // note that prev == 0 the first time (so adjust = 0)
      (acc + curr - (if curr > prev then 2 * prev else 0), curr)
  .acc


def romanToIntNaive(roman: IndexedSeq[RomanDigit]): IndexedSeq[Index] =
  roman.map(_.toInt)


val examples = List(
  "I" -> 1,
  "II" -> 2,
  "III" -> 3,
  "IV" -> 4,
  "LVIII" -> 58,
  "DCCLXXXIX" -> 789,
)


@main def run13(): Unit =
  for (input, expected) <- examples do
//    val result = romanToInt(stringToRoman(input))
    val result = romanToIntFold(input)
    assert(result == expected, s"$input | $result | expected: $expected")
  println("Ok")

// object RomanToIntBenchmark extends Bench.LocalTime:
//   val sizes = Gen.range("size")(300000, 1500000, 300000)

//   val ranges = for {
//     size <- sizes
//   } yield 0 until size

//   performance of "Range" in {
//     measure method "map" in {
//       using(ranges) in {
//         r => r.map(_ + 1)
//       }
//     }
//   }
