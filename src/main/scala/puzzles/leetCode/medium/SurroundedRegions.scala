package puzzles.leetCode.medium

val O = 'O'
val S = 'S'
val X = 'X'

def perimeter2(rMax: Int, cMax: Int) = {
  val topBottom = for { r <- 0 until rMax; c <- Vector(0, cMax - 1) } yield (r, c)
  val leftRight = for { r <- Vector(0, rMax - 1); c <- 1 until (cMax - 1) } yield (r, c)
  topBottom ++ leftRight
}

def perimeter(rMax: Int, cMax: Int)(f: (Int, Int) => Unit) = {
  val bottom = rMax - 1
  val right  = cMax - 1
  for (c <- 0 to right)  f(0, c)
  for (r <- 1 to bottom) f(r, right)
  for (c <- (right  - 1) to 0 by -1) f(bottom, c)
  for (r <- (bottom - 1) to 1 by -1) f(r, 0)
}

type Board = Array[Array[Char]]

// Use preorder DFS
def markSafeRegion(board: Board, fromTo: (Char, Char), r: Int, c: Int): Unit =
  if (0 <= r && r < board.length && 
      0 <= c && c < board(0).length && 
      board(r)(c) == fromTo._1
    ) {
    board(r)(c) = fromTo._2
    for (t <- List((0, 1), (1, 0), (0, -1), (-1, 0)))
      markSafeRegion(board, fromTo, r + t._1, c + t._2)
    // markSafeRegion(board, fromTo, r + 1, c)
    // markSafeRegion(board, fromTo, r - 1, c)
    // markSafeRegion(board, fromTo, r, c + 1)
    // markSafeRegion(board, fromTo, r, c - 1)
  }

  
def update(board: Board)(f: (Int, Int) => Unit): Unit =
  for {
    r <- board.indices
    c <- board(0).indices
  } f(r, c)

def solve(board: Board): Unit = {
  perimeter2(board.length, board(0).length).foreach { case (r, c) => 
    markSafeRegion(board, O -> S, r, c)
  }

  // perimeter(board.length, board(0).length) { (r, c) =>
  //   markSafeRegion(board, O -> S, r, c)
  // }

  update(board) { (r, c) => 
    if (board(r)(c) == O)
      board(r)(c) = X 
    else if (board(r)(c) == S) 
      board(r)(c) = O
  }
}

import zio.json.*
@main def mainSR =
  val board = 
    """[["X","X","X","X"],
        ["X","O","O","X"],
        ["X","X","O","X"],
        ["X","O","X","X"]]""".fromJson[Array[Array[Char]]].getOrElse(???)
// Output: [["X","X","X","X"],["X","X","X","X"],["X","X","X","X"],["X","O","X","X"]]"""
  println("-------------")
  board.foreach(r => println(r.toList))
  println("-------------")

  // perimeter(board.length, board(0).length) { (r, c) => println((r, c, board(r)(c))) }
  solve(board)

  println("-------------")
  board.foreach(r => println(r.toList))
  println("-------------")
