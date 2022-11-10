package puzzles.leetCode.medium


def normalize(w: String): String = w.sorted

def groupAnagrams(strs: Array[String]): List[List[String]] = {
  strs.toList.groupBy(normalize).values.toList
}

@main def mainGroupAnagrams =
  val strs = Array("eat","tea","tan","ate","nat","bat")
  println {
    groupAnagrams(strs)
  }

