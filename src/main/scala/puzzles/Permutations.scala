package puzzles


def toFunc[A](f: List[A]): Int => A = 
    f


def permutations[A]: List[A] => List[List[A]] =
  case Nil => List(List())
  case h :: t => 
    for 
      pt <- permutations(t)
      is <- pt.indices.inclusive.map(pt.splitAt).map { case (a, b) => a ++ (h :: b) }
    yield
      is
  


@main 
def permutationsMain =
  println {
    // permutations(List())
    // permutations(List('a'))
    //  permutations(List('a', 'b'))
    // permutations(List('a', 'b', 'c'))
    // permutations(List(1, 2, 3))
    // permutations(List('a', 'b', 'c', 'd'))
  }
  permutations(List(1,2,3)).map(l => l.mkString(",")).sorted.foreach(println)
