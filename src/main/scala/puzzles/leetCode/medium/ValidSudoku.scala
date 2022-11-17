package puzzles.leetCode.medium

import scala.collection.mutable

object SSolution_isValidSudokuolution extends App {

  val dot = '.'

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    val n = 9

    val rows = Array.fill(n)(Set.empty[Char])
    val cols = Array.fill(n)(Set.empty[Char])
    val boxes = Array.fill(3,3)(Set.empty[Char])

    for {
      r <- 0 until n
      c <- 0 until n
      if board(r)(c) != dot
      elem = board(r)(c)
    } yield
      val pos = board(r)(c) - 1
      if (rows(r).contains(elem))
        return false

    ???
    
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
      Array('8','3','.','.','7','.','.','.','.'),
      Array('6','.','.','1','9','5','.','.','.'),
      Array('.','9','8','.','.','.','.','6','.'),
      Array('8','.','.','.','6','.','.','.','3'),
      Array('4','.','.','8','.','3','.','.','1'),
      Array('7','.','.','.','2','.','.','.','6'),
      Array('.','6','.','.','.','.','2','8','.'),
      Array('.','.','.','4','1','9','.','.','5'),
      Array('.','.','.','.','8','.','.','7','9'),
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
  println(isValidSudoku(case1))
  println(isValidSudoku(case2))
  println(isValidSudoku(case3))
}

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
