package puzzles.leetCode

import org.scalameter.api.*

// L  V I I I
// 50 5 1 1 1
// -  - - - -

// I : Add 1 OR Subtract 1 to next (V OR X)
// X : Add 10 OR Subtract 10 to next (L OR C)
// C : Add 100 OR Subtract 100 to next (D OR M)

// 0.   1. 2.    3. 4.  5 6
// M    C   M    X  C   I V
// 1000 100 1000 10 100 1 5
// -    -   +    -  +   - +   
// 100 + 100 + (1000-2*100) + 10 + (100-2*10) + 1 + (5 - 2*1)
// 100 + 900 + 90 + 4

//     1000 +  (1000 - 100) + (100 - 10) + (5 - 1)


// (0, 1)  (M, C, -)   -> total + v(M)
// (1, 2)  (C, M, +)   -> total + v(M) - v(C)
// (2, 3)  (M, X, -)   -> skip
// (3, 4)  (X, C, +)   -> total + v(C) - v(X)
// (4, 5)  (C, I, -)   -> skip
// (5, 6)  (I, V, +)   -> total + v(V) - v(I)

/* 
LVIII

(L, V) total + v(L)
(V, I) total + v(V)
(I, I) total + V(I)
(I, I) total + V(I)
(I,  ) total + V(I)


*/


val value = 
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

  // l = roman.length
  // Index: 0 < i < l
  // Index.minValue = 0
  // Index.maxValue = l - 1
  def process (s: String, i: Index, total: Int) : Int = {
    val endPos = s.length
    if (i <= endPos) 
      val left = s(i - 1)
      if (i < endPos) {
        val right = s(i)
        if (value(left) >= value(right))
          process(s, i = i + 1, total = total + value(left))
        else
          process(s, i = i + 2, total = total + value(right) - value(left))
      } else
        total + value(left)
    else 
        total

  }


def romanToInt (s: String): Int = {
  if (s.length <= 1)
    value (s(0))
  else
    process (s = s, i = 1, total = 0)    
}

type Pos = Int

def romanToInt2 (s: String, i: Pos = 0, total: Int = 0): Int =
  if i < s.length then
    if i + 1 < s.length && value(s(i)) < value(s(i + 1)) then
      romanToInt2(s, i + 2, total + value(s(i + 1)) - value(s(i)))
    else
      romanToInt2(s, i + 1, total + value(s(i)))
  else
    total


val examples = List(
  "I" -> 1,
  "II" -> 2,
  "III" -> 3,
  "IV" -> 4,
  "LVIII" -> 58,
)


@main def run =
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