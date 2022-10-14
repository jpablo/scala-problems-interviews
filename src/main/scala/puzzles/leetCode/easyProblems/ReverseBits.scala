package puzzles.leetCode


def reverseBits(x: Long): Long = {
  var ret = 0L
  var power = 31
  var n = x
  while (n > 0) {
    // n & 1: get right-most bit
    // << power : shift `power` positions to the left (i.e. raise to 2*power)
    // ret += just sets the bit (it's like AND)
    ret += (n & 1) << power
    // drop right-most bit (divide by 2)
    n = n >> 1
    power -= 1
  }
  ret
    
}


@main def mainReverseBits =
  println(reverseBits(4294967293L))

// 1111111111111111111111111111111  
// 