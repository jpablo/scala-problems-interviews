package puzzles.leetCode


def freq(s: String) =
  s.foldLeft(Map.empty[Char,Int].withDefaultValue(0))((m,c) => m + (c -> (m(c) + 1)))

def isAnagram(s: String, t: String): Boolean =
  freq(s) == freq(t)

