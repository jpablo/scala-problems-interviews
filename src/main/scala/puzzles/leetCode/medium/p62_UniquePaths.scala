package puzzles.leetCode.medium

import collection.mutable.{Map => MMap}


// https://leetcode.com/problems/unique-paths/

def uniquePaths0(m: Int, n: Int, cache: MMap[(Int, Int), Int] = MMap.empty): Int = {
  val key = (m, n)
  val d = m * n
  if (d == 0 || d == 1)
    d
  else
    cache.getOrElseUpdate(key, uniquePaths0(m - 1, n, cache) + uniquePaths0(m, n - 1, cache))
}


def uniquePaths(m: Int, n: Int): Int = {
  val d = Array.fill(m, n)(1)
  for (r <- 1 until m; c <- 1 until n)
    d(r)(c) = d(r)(c - 1) + d(r - 1)(c)

  d(m - 1)(n - 1)
}