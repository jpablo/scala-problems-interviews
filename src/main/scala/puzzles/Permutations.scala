package puzzles


def permutations[A](lst: List[A]): List[List[A]] =
  println(lst)
  lst match
    case Nil => List(List.empty)

    case h :: t => 
      for 
        pt <- permutations(t)
        (a, b) <- pt.indices.inclusive.map(pt.splitAt)
      yield
        a ++ (h :: b)
  


@main 
def permutationsMain =
  // println {
    // permutations(List())
    // permutations(List('a'))
    //  permutations(List('a', 'b'))
    // permutations(List('a', 'b', 'c'))
    // permutations(List(1, 2, 3))
    // permutations(List('a', 'b', 'c', 'd'))
  // }
  permutations(List(1,2,3)).map(l => l.mkString(",")).sorted.foreach(println)
