package puzzles.leetCode


// 70
// https://leetcode.com/problems/climbing-stairs/

/* 
You are climbing a staircase. It takes n steps to reach the top.

Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
*/


/* 
Ideas:
  - Atras para adelante
  - DP

 */

def climbStairs(n: Int): Int = {
  def loop(cur: Int = 1, prev: Int = 1, i: Int = n): Int =
    if (i < 2)
      cur
    else
      loop(cur + prev, cur, i - 1)          
  loop()
}


def climb_Stairs0(i: Int, n: Int): Int =
  if (i > n)
    0
  else if (i == n)
    1
  else 
    val a = climb_Stairs0(i + 1, n)
    val b = climb_Stairs0(i + 2, n)
    println((a, b))
    a + b



@main def main70 =
  println(climb_Stairs0(0, 3))
