package puzzles.leetCode.easyProblems

import puzzles.in

import scala.annotation.tailrec
import collection.mutable.Map as MMap

// 70
// https://leetcode.com/problems/climbing-stairs/

/*
You are climbing a staircase. It takes n steps to reach the top.

Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
 */

/*
Ideas:
  - back to front: the known case is the last position!
  - DP

 */

// recursive with indices n ... 0
def climbRec1(n: Int, f: (Int, Int) => Int = _ + _): Int =
  if n < 2 then 1
  else
    f(climbRec1(n - 1, f), climbRec1(n - 2, f))

// tail recursive version
// cur ~ climbRec1(n)
// prev ~ climbRec1(n - 1)
def climbStairs(n0: Int, f: (Int, Int) => Int): Int =
  @tailrec
  def loop(cur: Int, prev: Int, n: Int): Int =
    if n < 2 then cur
    else
      loop(
        cur  = f(cur, prev),
        // by definition:
        // f(n) = f(n-1) + f(n-2)
        // so if we have f for two consecutive numbers (n-1) and (n-2) we can calculate f(n)
        prev = cur,
        n    = n - 1
      )
  loop(cur = 1, prev = 1, n = n0)
  // loop(cur = 1, 1, 3) =
  // loop(cur = 2, 1, 2) =
  // loop(cur = 3, 2, 1) =
  // 3


// recursive with indices 0 ... n
def climbRec0(i: Int, n: Int): Int =
  if i > n then 0
  else if i == n then 1
  else climbRec0(i + 1, n) + climbRec0(i + 2, n)

// memoized version of the above
def climbStairsMemo(i: Int, n: Int, cache: MMap[Int, Int]): Int =
  if i > n then 0
  else if i in cache then cache(i)
  else if i == n then 1
  else
    cache(i) = climbStairsMemo(i + 1, n, cache) + climbStairsMemo(i + 2, n, cache)
    cache(i)

def climbStairsArray(n: Int): Int =
  val cache = Array.ofDim[Int](n + 1)
  // set last entry to 1 as we are going to go back to front
  cache(cache.length - 1) = 1
  for i <- cache.indices.reverse do
    if i - 1 >= 0 then
      cache(i - 1) += cache(i)
    if i - 2 >= 0 then
      cache(i - 2) += cache(i)
  cache(0)

@main def main70(): Unit =
  println(climbStairsMemo(0, 3, MMap.empty))
  println(climbStairsArray(3))
