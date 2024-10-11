package puzzles.leetCode.easyProblems

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

def climbStairs(n: Int): Int =
  @tailrec
  def loop(cur: Int, prev: Int, i: Int): Int =
    if i < 2 then cur
    else
      loop(cur = cur + prev, prev = cur, i = i - 1)
  loop(1, 1, n)

def climbStairsMemo(i: Int, n: Int, cache: MMap[Int, Int] = MMap.empty): Int =
  if i > n then
    0
  else if cache.contains(i) then
    cache(i)
  else if (i == n)
    1
  else
    cache(i) = climbStairsMemo(i + 1, n, cache) + climbStairsMemo(i + 2, n, cache)
    cache(i)

def climbStairsArray(n: Int): Int =
  val solutions = Array.ofDim[Int](n + 1)
  solutions(solutions.length - 1) = 1
  for i <- solutions.indices.reverse do
    if i - 1 >= 0 then
      solutions(i - 1) += solutions(i)
    if i - 2 >= 0 then
      solutions(i - 2) += solutions(i)
  solutions(0)

@main def main70(): Unit =
  println(climbStairsMemo(0, 3))
  println(climbStairsArray(3))
