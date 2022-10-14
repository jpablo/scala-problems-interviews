package puzzles.leetCode


def climbStairs(n: Int): Int = {
  def go(cur: Int = 1, prev: Int = 1, i: Int = n): Int =
    if (i < 2)
      cur
    else
      go(cur + prev, cur, i - 1)          
  go()
}
