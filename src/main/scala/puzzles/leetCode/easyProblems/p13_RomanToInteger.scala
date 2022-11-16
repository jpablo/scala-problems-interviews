package puzzles.leetCode

// 13
// https://leetcode.com/problems/roman-to-integer/

// import org.scalameter.api.*
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
// For example, 2 is written as II in Roman numeral, just two ones added together. 12 is written as XII, 
// which is simply X + II. The number 27 is written as XXVII, which is XX + V + II.

// Roman numerals are usually written largest to smallest from left to right. 
// However, the numeral for four is not IIII. Instead, the number four is written as IV. 
// Because the one is before the five we subtract it making four. The same principle applies to the number nine, which is written as IX. 
// There are six instances where subtraction is used:

// I can be placed before V (5) and X (10) to make 4 and 9. 
// X can be placed before L (50) and C (100) to make 40 and 90. 
// C can be placed before D (500) and M (1000) to make 400 and 900.
// Given a roman numeral, convert it to an integer.

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
def romanToInt(roman: String, i: Index = 0, acc: Int = 0): Int =
  if (i >= roman.length)
    acc
  else {
    val curr = toInt(roman(i))
    lazy val next = toInt(roman(i + 1))
    // normal case: decreasing or equal
    if (i + 1 >= roman.length || curr >= next)
      // add current number
      romanToInt(roman, i + 1, acc + curr)
    else
      // exception: increasing
      // subtract current number to next one
      romanToInt(roman, i + 2, acc + next - curr)
  }


val examples = List(
  "I"     -> 1,
  "II"    -> 2,
  "III"   -> 3,
  "IV"    -> 4,
  "LVIII" -> 58,
)


@main def run13 =
  for (input, expected) <- examples do
    val result = romanToInt(input)
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