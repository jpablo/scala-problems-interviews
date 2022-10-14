package puzzles.leetCode



// Using a Boolean Array
def missingNumber0(nums: Array[Int]): Int = {
  val found = Array.ofDim[Boolean](nums.length + 1)
  for (i <- nums.indices) {
    found(nums(i)) = true
  }
  found.indexOf(false)
}

def log2(x: Int) = math.log10(x) / math.log10(2)

def missingNumber1(nums: Array[Int]): Int = {
  var expected = 0
  var actual = 0
  for (i <- (0 to nums.length)) expected ^= i // 0 ^ 0 ^ 1 ^ 2 ^ 3
  for (i <- nums.indices) actual ^= nums(i)   // 0 ^ 3 ^ 0 ^ 1
  expected ^ actual                           // 2
}

// use some properties of XOR

def missingNumber(nums: Array[Int]): Int = {
  var missing = nums.length
  for (i <- nums.indices) {
    missing ^= i ^ nums(i) // (3 ^ (0 ^ 3)) ^ (1 ^ 0) ^ (2 ^ 1)
  }
  missing
}



@main def mainMissingNumber =
  println(missingNumber(Array(3,0,1)))
  // println(missingNumber(Array(0,1)))
  // println(missingNumber(Array(9,6,4,2,3,5,7,0,1)))