package puzzles.leetCode


// Suppose you have a collection of segments, they may represent time intervals
// where a speaker was talking during a conference or meeting, each associated
// with a tag:
// A ->   |==========|
// B ->        |==|
// C ->     |================|
// ...
//
// Each segment has a starting point and and ending point.
// Also suposse, for simplicity, that each tag is mapped to only one segment.
//
// Create an index of tags by segment such that:
// - Each key in the index is unique and has no intersection with any other key
// - If an original segment S was tagged T, the union of all segments pointing
//   to T should cover S (there should be no sub segment of S that is not a
//   subsegment of a segment pointing to T).
//
// For example, given 3 segments:
//        0    1    2    3    4    5    6
// A ->   |===================|
// B ->             |====|
// C ->        |========================|
//
// Which can be written as:
// [ A -> (0, 4)
// , B -> (2, 3)
// , C -> (1, 6)
// ]
//
// The resulting map would be:
//
// [ (0, 1) -> {A}
// , (1, 2) -> {A, C}
// , (2, 3) -> {A, B, C}
// , (3, 4) -> {A, C}
// , (4, 6) -> {C}
// ]
//
// One way of thinking about this is in a graphical representation is:
//
//     |====|====|====|====|
//          |====|====|====|=====|
//               |====|
//
//        |    |    |    |    |
//        v    v    v    v    v
//
//     |====|====|====|====|=====|
//        |    |    |    |    |
//        v    v    v    v    v



//     |====|============|=====|
//               |====|




type Interval =  (Int, Int)

def mergeTagged(intervals: Map[String, Interval]): Map[Interval, Set[String]] = {
  val sorted: List[(String, Interval)] = intervals.toList.sortBy(_._2._1)

  val z0: List[(Interval, Set[String])] = List.empty

  sorted.foldLeft(z0) { case (((a, b), tags) :: rest, (tag, (l, r))) =>
    ???  
  }

  ???

}


// @main def mainFO =
//   println(merge("[[1,4],[0,4]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
//   println(merge("[[1,4],[0,5]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))
//   println(merge("[[2,3],[4,5],[6,7],[8,9],[1,10]]".fromJson[Array[Array[Int]]].right.get).toList.map(_.toList))