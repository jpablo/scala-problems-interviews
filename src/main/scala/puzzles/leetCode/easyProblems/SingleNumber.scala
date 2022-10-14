package puzzles.leetCode


def singleNumber(nums: Array[Int]): Int = {
  val idx = Array.ofDim[Boolean](60000)
  val min = 3000
  var i = 0
  while (i < nums.length) {
    idx(nums(i)+min) = !idx(nums(i)+min)
    i += 1
  }
  var j = 0
  while (j < nums.length && !idx(nums(j)+min)) j += 1
  nums(j)
}

def singleNumber2(nums: Array[Int]): Int = {
  var i = 0
  var a = 0
  while (i < nums.length) {
    a ^= nums(i)
    i += 1
  }
  a
}

@main def mainSingleNumber =
  println(singleNumber2(Array(2,2,1)))
  println(singleNumber2(Array(4,1,2,1,2)))
  println(singleNumber2(Array(1)))
  println(singleNumber2(Array(-1)))
