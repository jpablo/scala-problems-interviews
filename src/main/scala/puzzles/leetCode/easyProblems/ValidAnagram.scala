package puzzles.leetCode



def isAnagram(s: String, t: String): Boolean = {
  s.groupBy(identity).transform((_, ss) => ss.length) == t.groupBy(identity).transform((_, ss) => ss.length)
}

