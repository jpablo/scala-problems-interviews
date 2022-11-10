package puzzles.leetCode.medium

import scala.annotation.tailrec
import scala.language.unsafeNulls

object LongestPalindromicSubstring extends App {

  type Interval = (Int, Int)
  
  def longestPalindrome(s: String): String = {
    
    def len(i: Interval) = i._2 - i._1
    def max(a: Interval, b: Interval) = if (len(a) >= len(b)) a else b
    def predicate(ss: String) = ss.reverse == ss

    
    @tailrec
    def go(current: Interval, longest: Interval): Interval = {
      println(current)
      val (a, b) = current
      if (b >= s.length)
        longest
      else {
        println(s.substring(a, b))
        val next  = 
          if (!predicate(s.substring(a, b)))
            (a + 1, b)
          else 
            (a, b + 1)
        go(next, max(longest, next))
      }
    }
    
    val (a, b) = go((0, 0), (0, 0))
    s.substring(a, b)
  }

  println(longestPalindrome("babad"))
  //                           | 
}

