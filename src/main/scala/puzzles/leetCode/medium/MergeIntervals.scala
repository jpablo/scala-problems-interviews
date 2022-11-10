package puzzles.leetCode.medium

import zio.json.*

/*
- sort by start point

*/

def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
  val sorted = intervals.sortBy(_.head)
  var output = List(sorted.head)
  for (int@ Array(start, end) <- sorted.tail) {
    val Array(lastStart, lastEnd) = output.head
    if (start <= lastEnd)
      output = Array(lastStart, math.max(lastEnd, end)) :: output.tail
    else
      output ::= int
  }      
  output.reverse.toArray
}


@main def mainFO =
  println(merge("[[1,4],[0,4]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
  println(merge("[[1,4],[0,5]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
  println(merge("[[2,3],[4,5],[6,7],[8,9],[1,10]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))