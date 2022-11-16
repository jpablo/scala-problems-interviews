package puzzles.leetCode

// 66
// https://leetcode.com/problems/plus-one/


// You are given a large integer represented as an integer array digits, where each digits[i] is the ith digit of the integer. 
// The digits are ordered from most significant to least significant in left-to-right order. 
// The large integer does not contain any leading 0's.

// Increment the large integer by one and return the resulting array of digits.

/* 

Ideas:
- atras para adelante
- leer descripción con cuidado
- analisis de casos: extremo derecho es 9 o no

 */ 


def plusOne(digits: Array[Int]): Array[Int] = {
  def loop(r: Int): Unit =
    if (r >= 0) {
      digits(r) = (digits(r) + 1) % 10
      if (digits(r) == 0)
        loop(r - 1)
    }
  loop(digits.length - 1)
  if (digits.head == 0)
    1 +: digits
  else 
    digits
}

@main def main21 =
  println(plusOne(Array(1,2,3)).toList)
  println(plusOne(Array(1, 9)).toList)
  println(plusOne(Array(1, 9, 2)).toList)
  println(plusOne(Array(1, 9, 9)).toList)
  println(plusOne(Array(9)).toList)