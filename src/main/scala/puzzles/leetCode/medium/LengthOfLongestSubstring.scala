package puzzles.leetCode.medium

import scala.annotation.tailrec


// Trick: Sliding window

object SolutionLengthOfLongestSubstring extends App {

  type Interval = (Int, Int)
  
  def lengthOfLongestSubstring(s: String): Int = {
    
    def len(i: Interval) = i._2 - i._1
    def max(a: Interval, b: Interval) = if (len(a) >= len(b)) a else b
    
    @tailrec
    def go(current: Interval, chars: Set[Char], longest: Interval): Interval = {
      val (a, b) = current
      if (b >= s.length)
        longest
      else {
        val (next, chars2) = 
          if (chars contains s(b))
            ((a + 1, b), chars - s(a))
          else 
            ((a, b + 1), chars + s(b))
        go(next, chars2, max(longest, next))
      }
    }
    
    len(go((0, 0), Set.empty, (0, 0)))
  }

  println(lengthOfLongestSubstring("abcabcbb"))
}
