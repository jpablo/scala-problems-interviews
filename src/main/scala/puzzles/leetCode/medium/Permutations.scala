package puzzles.leetCode.medium


/* Biyections of a set onto itself */


/*
- Structural recursion 
- Divide and conquer

- Calculate the permutations of a smaller list (the tail)
- There are (n - 1)! permutations of the tail
- For each one, the head `h` could be inserted into `n` positions

- key function: splitAt

*/

def permute(nums: Array[Int]): List[List[Int]] = {
  if (nums.isEmpty) 
    List(List()) // this is the empty permutation: f: Empty => Empty
  else {
    val (h, t) = nums.splitAt(1)
    for {
      pt0 <- permute(t) // length: n - 1
      pt1 <- pt0.indices.inclusive.map(pt0.splitAt).map { case (l, r) => l ++ h ++ r } // length: n
    } yield
      pt1
  }
}


@main def mainPermute =
  println {
    // permute(Array())
    // permute(Array(1))
    // permute(Array(1,2))
    // permute(Array(1,2,3))
  }
  println(permute(Array()))
  println(permute(Array(2)))
  println(permute(Array(1, 2)))
  