package puzzles.leetCode.medium

import scala.collection.mutable


/* 
Idea:
  - Iterate over all (r, c)
  - Sets for: rows, columns, boxes
  - fail the moment a duplicate is found

*/

val dot = '.'

def isValidSudoku(board: Array[Array[Char]]): Boolean = {
  val n = 9
  val rows  = Array.fill(n)(mutable.Set.empty[Char])
  val cols  = Array.fill(n)(mutable.Set.empty[Char])
  val boxes = Array.fill(3,3)(mutable.Set.empty[Char])
  var r = 0
  var c = 0
  var valid = true
  while (r < n && valid) {
    val elem = board(r)(c)
    if (elem != dot) {
      valid  = rows(r).add(elem)
      valid &= cols(c).add(elem)
      valid &= boxes(r/3)(c/3).add(elem)
    }
    c += 1; c %= n
    r += (if (c == 0) 1 else 0)
  }
  valid
}

// true
val case1 = 
  Array(
    Array('5','3','.', '.','7','.', '.','.','.'),
    Array('6','.','.', '1','9','5', '.','.','.'),
    Array('.','9','8', '.','.','.', '.','6','.'),
      
    Array('8','.','.', '.','6','.', '.','.','3'),
    Array('4','.','.', '8','.','3', '.','.','1'),
    Array('7','.','.', '.','2','.', '.','.','6'),
      
    Array('.','6','.', '.','.','.', '2','8','.'),
    Array('.','.','.', '4','1','9', '.','.','5'),
    Array('.','.','.', '.','8','.', '.','7','9'),
  )

// false
val case2 =
  Array(
    Array('8','3','.', '.','7','.', '.','.','.'),
    Array('6','.','.', '1','9','5', '.','.','.'),
    Array('.','9','8', '.','.','.', '.','6','.'),

    Array('8','.','.', '.','6','.', '.','.','3'),
    Array('4','.','.', '8','.','3', '.','.','1'),
    Array('7','.','.', '.','2','.', '.','.','6'),

    Array('.','6','.', '.','.','.', '2','8','.'),
    Array('.','.','.', '4','1','9', '.','.','5'),
    Array('.','.','.', '.','8','.', '.','7','9'),
  )
// false
val case3 =
  Array(
    Array('.','.','4', '.','.','.', '6','3','.'),
    Array('.','.','.', '.','.','.', '.','.','.'),
    Array('5','.','.', '.','.','.', '.','9','.'),
    
    Array('.','.','.', '5','6','.', '.','.','.'),
    Array('4','.','3', '.','.','.', '.','.','1'),
    Array('.','.','.', '7','.','.', '.','.','.'),
    
    Array('.','.','.', '5','.','.', '.','.','.'),
    Array('.','.','.', '.','.','.', '.','.','.'),
    Array('.','.','.', '.','.','.', '.','.','.'),
  )


@main def main36 =
  assert(isValidSudoku(case1))
  assert(!isValidSudoku(case2))
  assert(!isValidSudoku(case3))
  println("Ok")


// scala> (0 to 8).map(r => (0 to 8).map(c => s'${r/3}${c/3}')).foreach(println)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
