package puzzles

import scala.annotation.tailrec
import collection.mutable.Map as MMap

/*
1 2 3
4 5 6
7 8 9
* 0 #
 */


val nextMoves = Map(
  1 -> List(6, 8),
  2 -> List(7, 9),
  3 -> List(4, 8),
  4 -> List(0, 3, 9),
  5 -> List(),
  6 -> List(0, 1, 7),
  7 -> List(2, 6),
  8 -> List(1, 3),
  9 -> List(4, 2),
  0 -> List(4, 6),
)

def toString(ll: List[List[Int]]) =
  ll.map(_.mkString).mkString("[", ",", "]")



// DP + Cache
def allDigitsRec(digit: Int, n: Int, cache: MMap[(Int, Int), List[List[Int]]] = MMap.empty): List[List[Int]] =
  val key = (digit, n)
  // println(s"allDigitsRec($digit, $n)")
  if cache.contains(key) then
    cache(key)
  else
    val result =
      if n <= 1 then
        List(List(digit))
      else
        for
          m <- nextMoves(digit)
          p <- allDigitsRec(m, n - 1, cache)
        yield
          digit :: p
    // println((n, toString(combined)))
    cache.getOrElseUpdate(key, result)


// Idea:
// Prepend the next number to each elem in numbers
// (BFS)
def allDigits(i: Int, n0: Int): List[List[Int]] =
  @tailrec
  def loop(n: Int, result: List[List[Int]]): List[List[Int]] =
    if n >= n0 then
      result
    else
      val acc =
        for
          number <- result
          moves = nextMoves(number.head)
          next <- moves.map(_ :: number)
        yield
          next
      loop(n + 1, acc)
  loop(1, List(List(i))).map(_.reverse)


@main def mainPAD =
  val m = 5
  val n1 = allDigits(1, m)
  val n2 = allDigitsRec(1, m)
  assert(n1 == n2)
  println("Ok")
  // println(n1)
  // println(n2)
  // (0 to 9).map(allDigits).map(_.length).foreach(println)
