package puzzles.leetCode

type Pos = Int
def plusOne(digits: Array[Int]): Array[Int] = {
  // state:
  var r: Pos = digits.length - 1
  var done = false
  
  while (r >= 0 && !done) {
    if (digits(r) == 9) {
      digits(r) = 0
      r -= 1
    } else {
      digits(r) += 1
      done = true          
    }
  }
  
  if (done)
    digits
  else
    Array(1) ++ Array.fill(digits.length)(0)        
}

