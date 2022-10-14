package puzzles.leetCode

def titleToNumber(columnTitle: String): Int = {
  
  // 1. Convert string into a list of chars
  // "AB" -> ['A', 'B']
  // 2. Convert to numbers using 'A' as the first element 
  // base = A'.toInt + 1
  // ['A', 'B'].map(_.toInt - base) -> [1, 2]
  // 3. Convert the list of numbers to a single number like so
  // 'AA' -> [1, 1] -> 1*26 + 1
  // 'AB' -> [1, 2] -> 1*26 + 2
  // 'ZY' -> [26, 25] -> 26*26 + 25
  // 'XYZ' -> [X.toInt, Y.toInt, Z.toInt] = [x,y,z] -> x*26^2 + y*26^1 + z*26^0
  val zero = 'A'.toInt
  val base = 26
  val digits = columnTitle.reverse.map(_.toInt - zero + 1)
  digits.zipWithIndex.foldLeft(0) { case (acc, (d, i)) => 
    d * math.pow(base, i).toInt + acc 
  }
    
}
