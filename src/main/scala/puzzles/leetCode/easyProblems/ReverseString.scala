package puzzles.leetCode

def reverse(s: Array[Char]) =
  var i = 0
  def j(i: Int) = s.length - i - 1
  while (i < s.length && i < j(i)) {
    val t = s(i)
    s(i) = s(j(i))
    s(j(i)) = t
    i += 1
  }

