  package puzzles.leetCode


/* 
[0,1,0,3,12]
 i j
// j: first non zero
// i: first zero

// swap values of i, j
[1,0,0,3,12]
 i j

// move i -> j
[1,0,0,3,12]
   j
   i
// advance j to the next non-zero
[1,0,0,3,12]
   i   j

 // repeat  

 */



  object SolutionMoveZeroes extends App {

    val a1 = Array(0,1,0,3,12)
    val a2 = Array(0)
    val a3 = Array(1)
    val a4 = Array(1, 0)
    val a5 = Array(1, 0, 1)
    moveZeroes(a5)
    println(a5.toList)

    def moveZeroes(nums: Array[Int]): Unit = {
      var i = 0 // first zero 
      var j = 0 // first non-zero
      while (i < nums.length && j < nums.length && i <= j) {
        if (i == j && nums(i) != 0) {
          i += 1
          j += 1
        }
        else if (nums(j) == 0)
          j += 1
        else if (nums(i) != 0)
          i += 1
        else {
          nums(i) = nums(j)
          nums(j) = 0
        }
      }
    }

    def moveZeroes0(nums: Array[Int]): Unit = {
      def shiftLeft(i: Int, j: Int, d: Int) =  {
        for (k <- i until j)
          nums(k) = nums(k + d)
        nums(j) = 0
      }
      
      // two pointers, end to start
      var i = nums.length - 1
      var j = i
      while (i >= 0) {
        if (nums(i) == 0) {
          shiftLeft(i, j, 1)
          j -= 1
        }
        i -= 1
      }
  }

}