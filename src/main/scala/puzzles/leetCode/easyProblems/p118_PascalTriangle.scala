package puzzles.leetCode

import scala.annotation.tailrec

// 118
// https://leetcode.com/problems/pascals-triangle/


// Given an integer numRows, return the first numRows of Pascal's triangle.
// In Pascal's triangle, each number is the sum of the two numbers directly above it.


/* 
Idea:
 - fold over previous row
 - fold: return prev element as part of the accumulator
*/


def generate(numRows: Int): List[List[Int]] = {
  val z0 = (0, List.empty[Int])
  @tailrec def loop (i: Int, result: List[List[Int]]): List[List[Int]] = {
    if (i <= 1)
      result
    else {
      val prevRow = result.head
      val (_, next) = prevRow.foldLeft(z0) { case ((p, row), i) => (i, (p + i) :: row) }
      loop(i - 1, (1 :: next) :: result)
    }
  }
  loop(numRows, List(List(1))).reverse
}

@main def run118 =
  generate(5).foreach(println)