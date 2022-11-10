package puzzles.leetCode.medium

type Mat = Array[Array[Int]]
type Pair = (Int, Int)

implicit class MapOps(m: Mat) extends AnyVal { 
  def apply(p: Pair) = m(p._1)(p._2) 

  def update(p: Pair, v: Int) = m(p._1)(p._2) = v

  def swap(pairs: (Pair, Pair)) = {
    val (p1, p2) = pairs
    val v2 = m(p2)
    m.update(p2, m(p1))
    m.update(p1, v2)
  }
}

def modify(m: Mat, cols: Int => Range, rule: Pair => Pair): Unit =
  for { 
    r <- m.indices
    c <- cols(r)
  } 
    m.swap((r, c) -> rule((r, c)))

// def transpose(m: Mat): Unit =
//   for { 
//     row <- m.indices
//     col <- row until m.size // upper diagonal
//   } 
//     m.swap((row, col) -> (col, row))

// def reflect(m: Mat): Unit = {
//   val s = m.size
//   for {
//     row <- m.indices
//     col <- 0 until s / 2 // only left half
//   } 
//     m.swap((row, col) -> (row, s - col - 1))
// }

def rotate(matrix: Mat): Unit = {
  val s = matrix.size
  modify(matrix, row => row until s, _.swap)
  modify(matrix,   _ => 0 until s / 2 , { case (r, c) => (r, s - c - 1) })
  // transpose(matrix)
  // reflect(matrix)
}


@main def mainRotate =
  val m = Array(
    Array(1, 2, 3),
    Array(4, 5, 6),
    Array(7, 8, 9),
  )

  m.foreach(r => println(r.mkString(" ")))
  println("----")
  rotate(m)
  println("----")
  m.foreach(r => println(r.mkString(" ")))
