package puzzles

import collection.mutable.ArrayBuffer

type Index = Int
type Interval = (Int, Int)

class RangeUnion(args: Interval*):
  val intervals: ArrayBuffer[Interval] = ArrayBuffer(args*)
  
  def insert(start: Int, end: Int): Unit =
    getOverlaps(start, end) match
      case ((i, -1), _)      => intervals.insert(i, (start, end))
      case ((i, j), (a, b))  => intervals.patchInPlace(i, List(math.min(start, a) -> math.max(end, b)), j - i + 1)
  
  def getOverlaps(start: Int, end: Int): ((Index, Index), (Int, Int)) =
    val ignore = (-1, -1)
    val indices = intervals.zipWithIndex
    val left  = indices.find { case ((_, b), i) => start <= b }
    val right = indices.reverseIterator.find { case ((x, _), j) => x <= end }
    
    (left, right) match
      case (_,    None)    => (0, -1) -> ignore
      case (None, Some(_)) => (intervals.length, -1) -> ignore
      case (Some((a, _), i), Some((_, b), j)) => if i > j then (i, -1) -> ignore else (i, j) -> (a, b)

  def query(i: Int): Boolean =
    intervals.exists { (a, b) => a <= i && i <= b }


@main def mainRangeUnion =
  // val ru = RangeUnion(2 -> 5, 9 -> 13)
  // println(ru.getOverlaps(2, 3))
  // println(ru.getOverlaps(9, 10))
  // println(ru.getOverlaps(4, 10))
  // println(ru.getOverlaps(1, 3))
  // println(ru.getOverlaps(1, 6))
  // println(ru.getOverlaps(1, 9))
  // println(ru.getOverlaps(1, 15))
  // println(ru.getOverlaps(0, 1))
  // println(ru.getOverlaps(14, 15))
  // println(ru.getOverlaps(6, 7))

  val ru2 = RangeUnion()
  ru2.insert(9, 13)
  ru2.insert(2, 5)
  ru2.insert(6, 7)
  assert(ru2.query(0) == false)
  assert(ru2.query(2) == true)
  assert(ru2.query(10) == true)
  ru2.insert(14, 15)
  ru2.insert(0, 2)
  ru2.insert(-1, 14)

  println(ru2.intervals)