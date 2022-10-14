package puzzles.leetCode.medium

import scala.collection.mutable

object SSolution_isValidSudokuolution extends App {

  val dot = '.'

  def noDups(row: Array[Char]) = {
    var found = Set.empty[Char]
    var i = 0
    var dups = false
    while (i < row.length && !dups) {
      if (row(i) != dot) {
        if (found.contains(row(i))) {
            dups = true
        }
        found += row(i)
      }
      i += 1
    }
    !dups
  }

  def rowsOk(board: Array[Array[Char]]): Boolean = {
    board.forall(noDups)
  }

  def colsOk(board: Array[Array[Char]]): Boolean = {
    var c = 0
    var dups = false
    while (c < 9 && !dups) {
      var found = Set.empty[Char]
      var r = 0
      while (r < 9 && !dups) {
        // println(s"(r, c): ($r, $c)")
        val char = board(r)(c)
        if (char != dot) {
          // println(s"char: $char, found: $found")
          if (found contains char)
            dups = true
          else
            found += char
        }
        r += 1
      }
      c += 1
    }
    !dups
  }
  
  def squaresOk(board: Array[Array[Char]]): Boolean = {
    var dups = false
    var blocks = Map.empty[(Int, Int), Set[Char]].withDefaultValue(Set.empty)
    var r = 0
    while (r < 9 && !dups) {
      var c = 0
      while (c < 9 && !dups) {
        val char = board(r)(c)
        val block = (r/3, c/3)
        val found = blocks(block)
        if (char != dot) {
          if (found contains char)
            dups = true
          else
            blocks += (block -> (found + char))
        }
        c += 1
      }
      r += 1
    }
    !dups
  }

  def allOk(board: Array[Array[Char]]): Boolean = {
    var dups = false
    val rows = Array.fill(9)(mutable.Set.empty[Char])
    val cols = Array.fill(9)(mutable.Set.empty[Char])
    var blocks = (0 until 3).flatMap(i => (0 until 3).map(j => (i,j) -> mutable.Set.empty[Char])).toMap
    var r = 0
    while (r < 9 && !dups) {
      var c = 0
      while (c < 9 && !dups) {
        val char = board(r)(c)
        val b = (r/3, c/3)
        val block = blocks(b)
        val row = rows(r)
        val col = cols(c)
        if (char != dot) {
          val inBlock = block contains char
          val inRow = row contains char
          val inCol = col contains char
          if (inBlock || inRow || inCol)
            dups = true
          else {
            if (!inBlock)
              blocks += (b -> (block + char))
            if (!inRow)
              row += char
            if (!inCol)
              col += char
          }
        }
        c += 1
      }
      r += 1
    }
    !dups
  }

  def allOkTC(board: Array[Array[Char]]): Boolean = {
    
    case class State(
      rows: Map[Int, Set[Char]],
      cols: Map[Int, Set[Char]],
      blocks: Map[(Int, Int), Set[Char]]
    )
    // found duplicates:
    def go(r: Int = 0, c: Int = 0, state: State): Boolean = {
      if (r >= 9)
        false
      else if (c >= 9)
        go(r + 1, 0,state)
      else {
        val v = board(r)(c)
        if (v == dot)
          go(r, c + 1, state)
        else {
          val b = (r/3, c/3)
          val inBlock = state.blocks(b) contains v
          val inRow   = state.rows(r)   contains v
          val inCol   = state.cols(c)   contains v
          if (inBlock || inRow || inCol)
            true
          else {
            val state1 = if (!inBlock) state.copy (blocks = state.blocks + (b -> (state.blocks(b) + v))) else state
            val state2 = if (!inRow)   state1.copy(rows   = state1.rows  + (r -> (state.rows(r)   + v))) else state1
            val state3 = if (!inCol)   state2.copy(cols   = state2.cols  + (c -> (state.cols(c)   + v))) else state2
            go(r, c + 1, state3)
          }
        }
      }
    }
    val state = State(
      rows = (for i <- 0 until 9 yield i -> Set.empty[Char]).toMap,
      cols = (for i <- 0 until 9 yield i -> Set.empty[Char]).toMap,
      blocks = (for i <- 0 until 3; j <- 0 until 3 yield (i,j) -> Set.empty).toMap
    )
    !go(state = state)
  }

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    allOkTC(board)
  }

  // true
  val case1 = 
    Array(
      Array('5','3','.', '.','7','.', '.','.','.'),
      Array('6','.','.', '1','9','5', '.','.','.'),
      Array('.','9','8', '.','.','.', '.','6','.'),
       
      Array('8','.','.', '.','6','.', '.','.','3'),
      Array('4','.','.', '8','.','3', '.','.','1'),
      Array('7','.','.', '.','2','.', '.','.','6'),
       
      Array('.','6','.', '.','.','.', '2','8','.'),
      Array('.','.','.', '4','1','9', '.','.','5'),
      Array('.','.','.', '.','8','.', '.','7','9'),
    )

  // false
  val case2 =
    Array(
      Array('8','3','.','.','7','.','.','.','.'),
      Array('6','.','.','1','9','5','.','.','.'),
      Array('.','9','8','.','.','.','.','6','.'),
      Array('8','.','.','.','6','.','.','.','3'),
      Array('4','.','.','8','.','3','.','.','1'),
      Array('7','.','.','.','2','.','.','.','6'),
      Array('.','6','.','.','.','.','2','8','.'),
      Array('.','.','.','4','1','9','.','.','5'),
      Array('.','.','.','.','8','.','.','7','9'),
    )
  // false
  val case3 =
    Array(
      Array('.','.','4', '.','.','.', '6','3','.'),
      Array('.','.','.', '.','.','.', '.','.','.'),
      Array('5','.','.', '.','.','.', '.','9','.'),
      
      Array('.','.','.', '5','6','.', '.','.','.'),
      Array('4','.','3', '.','.','.', '.','.','1'),
      Array('.','.','.', '7','.','.', '.','.','.'),
      
      Array('.','.','.', '5','.','.', '.','.','.'),
      Array('.','.','.', '.','.','.', '.','.','.'),
      Array('.','.','.', '.','.','.', '.','.','.'),
    )
  println(isValidSudoku(case1))
  println(isValidSudoku(case2))
  println(isValidSudoku(case3))
}

// scala> (0 to 8).map(r => (0 to 8).map(c => s'${r/3}${c/3}')).foreach(println)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(00, 00, 00, 01, 01, 01, 02, 02, 02)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(10, 10, 10, 11, 11, 11, 12, 12, 12)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
// Vector(20, 20, 20, 21, 21, 21, 22, 22, 22)
