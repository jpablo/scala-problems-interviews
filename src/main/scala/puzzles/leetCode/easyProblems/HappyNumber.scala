package puzzles.leetCode.easyProblems

// 1. Find digits using repeated modulo and division (% 10 gives the last digit!)
// 2. Find cycles using a Set of observed values


def isHappy0(n: Int): Boolean = {

  def digitsSum (n: Int): Int =
    List.unfold(n) { i => if (i == 0) None else Some(((i % 10)*(i % 10), i / 10)) }.sum

  var sums = Set.empty[Int]
  var s = digitsSum(n)
  while (!sums.contains(s) && s != 1) {
    sums += s
    s = digitsSum(s)
  }

  s == 1
}


// Floyd's cycle finding algorithm: fast and slow pointers
def isHappy(n: Int): Boolean = {

  def pow(i: Int) = i * i

  def getNext (n: Int): Int =
    List.unfold(n) { i => if (i == 0) None else Some((pow(i % 10), i / 10)) }.sum

  var slow = n
  var fast = getNext(n)
  while (fast != 1 && slow != fast) {
    slow = getNext(slow)
    fast = getNext(getNext(fast))
  }

  fast == 1
}



@main def mainHappy =
  println(isHappy(19))
  println(isHappy(2))
