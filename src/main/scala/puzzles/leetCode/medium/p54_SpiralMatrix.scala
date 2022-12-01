package puzzles.leetCode.medium

import zio.json.*
import collection.mutable.ArrayBuffer


// https://leetcode.com/problems/spiral-matrix/

def repeat(n: Int)(body: => Unit) = 
  (0 until n).foreach(_ => body)


def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
  var result = List.empty[Int]
  var (rows, cols) = (matrix.size, matrix(0).size)
  var direction = 1
  // start to the left of the first cell
  var (r, c) = (0, -1)
  // finish when either rows or cols become zero
  while (rows * cols > 0) {
    // horizontal: single row
    repeat(cols) {
      c += direction
      result ::= matrix(r)(c)
    }
    // `c` is the last valid column
    // one less row to go
    rows -= 1
    // vertical: single column
    repeat(rows) {
      r += direction // start at next row
      result ::= matrix(r)(c)
    }
    // one less column to go
    cols -= 1
    // reverse direction
    direction *= -1
  }
  result.reverse
}

@main def mainSpiralOrder =
  assert(spiralOrder("[[1,2,3],[4,5,6],[7,8,9]]".fromJson[Array[Array[Int]]].right.get) == List(1, 2, 3, 6, 9, 8, 7, 4, 5))
  // println(spiralOrder("[[1,2,3,4],[5,6,7,8],[9,10,11,12]]".fromJson[Array[Array[Int]]].right.get))