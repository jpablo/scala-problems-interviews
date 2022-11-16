package puzzles


def canConstruct(target: String, words: List[String]): Boolean =
  val solutions = Array.ofDim[Boolean](target.length + 1)
  solutions(0) = true
  for
    i <- solutions.indices if solutions(i)
    w <- words if target.substring(i).nn.startsWith(w)
  do
    solutions(i + w.length) = true
  solutions.last

def countConstruct(target: String, words: List[String]): Int =
  val solutions = Array.ofDim[Int](target.length + 1)
  solutions(0) = 1
  for
    i <- solutions.indices if solutions(i) > 0
    w <- words if target.substring(i).nn.startsWith(w)
  do
    solutions(i + w.length) += solutions(i)
  solutions.last


def allConstruct(target: String, words: List[String]): List[List[String]] =
  val solutions = Array.fill[List[List[String]]](target.length + 1)(List.empty)
  solutions(0) = List(List.empty)
  for
    i <- solutions.indices if solutions(i).nonEmpty
    w <- words if target.substring(i).nn.startsWith(w)
  do
    solutions(i + w.length) ++= solutions(i).map(w :: _)
  solutions.last.map(_.reverse)



@main def mainCC =
  assert(canConstruct("abcdef", List("ab", "abc", "cd", "def", "abcd")))
  assert(canConstruct("skateboard", List("bo", "rd", "ate", "t", "ska", "sk", "boar")) == false)
  assert(canConstruct("enterapotentpot", List("a", "p", "ent", "enter", "ot", "o", "t")))
  assert(canConstruct("eeeeeeeeeeeeeeeeeeeeeeef", List("e", "ee", "eee", "eeee")) == false)
  println("Ok: canConstruct")

  assert(countConstruct("purple", List("purp", "p", "ur", "le", "purpl")) == 2)
  println("Ok: countConstruct")

  assert(allConstruct("purple", List("purp", "p", "ur", "le", "purpl")) == List(List("purp", "le"), List("p", "ur", "p", "le")))
  assert(allConstruct("abcdef", List("ab", "abc", "cd", "def", "abcd", "ef", "c")) == List(List("abc", "def"), List("ab", "c", "def"), List("abcd", "ef"), List("ab", "cd", "ef")))
  println("Ok: allConstruct")