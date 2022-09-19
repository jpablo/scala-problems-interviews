package puzzles



def insert [A] (a: A, i: Int) (ls: List[A]): List[A] =
  val (before, after) = ls.splitAt(i)
  before ++ (a :: after)


def insertAll [A] (a: A) (ls: List[A]): List[List[A]] =
  for i <- (0 to ls.length).toList yield
    insert (a, i) (ls)


def permutations [A] (input: List[A]): List[List[A]] =
  input match
    case Nil => List(List())
    case h :: t => permutations(t) flatMap insertAll(h)
  

object Test extends App:
  println {
    // permutations(List())
    // permutations(List('a'))
     permutations(List('a', 'b'))
    // permutations(List('a', 'b', 'c'))
    // permutations(List('a', 'b', 'c', 'd'))
  }
