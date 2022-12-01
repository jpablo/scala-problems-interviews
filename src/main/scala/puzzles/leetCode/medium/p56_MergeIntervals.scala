package puzzles.leetCode.medium

import zio.json.*

// https://leetcode.com/problems/merge-intervals/


// Given an array of intervals where intervals[i] = [starti, endi], merge all overlapping intervals, 
// and return an array of the non-overlapping intervals that cover all the intervals in the input.


/*
- sort by start point
- left to right, replace last inverval if needed

*/

def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
  val sorted = intervals.sortBy(_.head).map(Array(_))

  sorted.reduce { case (acc, Array(Array(s, e))) => 
    val Array(s0, e0) = acc.last
    if (s <= e0) 
      acc.init :+ Array(s0, math.max(e0, e)) 
    else 
      acc :+ Array(s, e)
  }

}


@main def mainFO =
  println(merge("[[1,4],[0,4]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
  println(merge("[[1,4],[0,5]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
  println(merge("[[2,3],[4,5],[6,7],[8,9],[1,10]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))