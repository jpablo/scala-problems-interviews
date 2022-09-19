package puzzles

import scala.annotation.tailrec

type Point = Int
type Points = Set[Point]
case class Segment(left: Point, right: Point)

/**
  * @param segments Sorted in increasing order by right end
  */
def segmentsCover(segments: List[Segment]): Points =

  @tailrec
  def go(pending: List[Segment], acc: Points): Points = pending match
    case Nil => acc
    case Segment(_, minRight) :: rest =>
      val segments1 = rest.filterNot { case Segment(l, r) => l <= minRight && minRight <= r }
      go(segments1, acc + minRight)

  go(segments.sortBy(_.right), Set.empty)




@main def runSegments() =
  val testCases = List(
    List((1, 3), (2, 5), (3, 6)) -> Set(3),
    List((4, 7), (1, 3), (2, 5), (5, 6)) -> Set(3, 6)
  )

  for (input, expected) <- testCases do
    val result = segmentsCover(input.map(Segment.apply))
    assert(expected == result, s"expected: $expected; found: $result")
