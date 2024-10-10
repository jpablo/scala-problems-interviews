package puzzles.leetCode.medium


// 139.
// https://leetcode.com/problems/word-break/

// Decision Tree -> Cache -> Dynamic Programming

def loop(i: Int, s: String, dp: Array[Boolean])(ws: List[String]): Unit = ws match {
  case w :: rest => 
    if (i + w.length <= s.length && s.substring(i, i + w.length) == w)
      dp(i) = dp(i + w.length)
      
    if (!dp(i))
      loop(i, s, dp)(rest)

  case Nil => 
    ()
}

def wordBreak(s: String, wordDict: List[String]): Boolean = {
  val dp = Array.ofDim[Boolean](s.length + 1)
  dp(s.length) = true
  for (i <- (s.length - 1) to 0 by -1)
    loop(i, s, dp)(wordDict)
  dp(0)
}

@main def main139 =
  println(wordBreak("leetcode", List("leet", "code")))
  println(wordBreak("applepenapple", List("apple","pen")))
  println(wordBreak("catsandog", List("cats","dog","sand","and","cat")))