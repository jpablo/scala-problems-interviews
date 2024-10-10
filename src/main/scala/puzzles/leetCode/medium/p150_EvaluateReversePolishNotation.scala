package puzzles.leetCode.medium

import scala.collection.mutable


// 150
// https://leetcode.com/problems/evaluate-reverse-polish-notation/

def binOp: Map[String, (Int, Int) => Int] = Map(
  "+" -> (_ + _),
  "-" -> (_ - _),
  "*" -> (_ * _),
  "/" -> (_ / _)
)

def evalRPN(tokens: Array[String]): Int = {
  var stack = mutable.ArrayBuffer.empty[Int]
  for (t <- tokens) {
    if (t.length > 1 || t(0).isDigit)
      t.toInt +=: stack
    else
      stack.patchInPlace(0, Seq(binOp(t)(stack(1), stack(0))), 2)
  }
  stack.head
}

@main def main150 =
  assert(evalRPN(Array("2","1","+","3","*")) == 9)
  assert(evalRPN(Array("4","13","5","/","+")) == 6)
  println("Ok")
