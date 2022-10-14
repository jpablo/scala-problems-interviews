package puzzles




def permutations[A]: List[A] => List[List[A]] =
  case Nil => List(List())
  case h :: t => 
    for 
      pt <- permutations(t)
      is <- pt.indices.inclusive.map(pt.splitAt).map { case (a, b) => a ++ (h :: b) }
    yield
      is
  

object Test extends App:
  println {
    // permutations(List())
    // permutations(List('a'))
    //  permutations(List('a', 'b'))
    permutations(List('a', 'b', 'c'))
    // permutations(List('a', 'b', 'c', 'd'))
  }
  // permutations(List(2, 3, 4, 5)).map(l => l.mkString(",")).foreach(println)
