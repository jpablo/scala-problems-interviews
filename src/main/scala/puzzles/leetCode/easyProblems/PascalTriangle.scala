package puzzles.leetCode


def generate(numRows: Int): List[List[Int]] = {
  def go (i: Int, acc: List[List[Int]]): List[List[Int]] = {
    if (i <= 1)
      acc
    else {
      val nextInit = acc.head.foldLeft((0, List.empty[Int])) { case ((i0, row), i1) => (i1, (i0 + i1) :: row) }
      var next     = (1 :: nextInit._2).reverse
      go(i - 1, next :: acc)
    }
  }
  go(numRows, List(List(1))).reverse

  // init = [[1]]
  // go(1, init) = init

  // go(2, init) = 
  // go(1, next :: [[1]]) = next :: [[1]]
  // go(1, [1,1] :: [[1]]) = [[1,1], [1]]
}


//      0 1 2 3
//   ---------
// 0: 0 1
// 1: 0 1 1
// 2: 0 1 2 1

// 3: 0 1 3 3   1

// 4: 0 1 4 6   4 1
// 5: 0 1 5 10 10 5 1
//

// a = [1, a1, ... ai, a(i+1), ... , 1]
// b = [1, a1 + 1, ..., ]
// a0 == ak == 1


// row(k)(0) = 1 for all k
// row(k)(k) = 1 for all j = k
// row(k)(k+j) = 0 for all j > 0

// row(k)(j) = row(k-1)(j) + row(k-1)(j-1)




@main def run2 =
  println(generate(4))