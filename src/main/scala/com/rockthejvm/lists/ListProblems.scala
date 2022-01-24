package com.rockthejvm.lists

import com.rockthejvm.lists.RList.from
import scala.util.Random
import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
    * Standard functions
    */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
  // get element at a given index

  def apply(index: Int): T

  def foldLeft[A](init: A)(step: (A, T) => A): A = {
    @tailrec
    def go(ts: RList[T], acc: A): A = ts match
      case RNil   => acc
      case h :: t => go(t, step(acc, h))

    go(this, init)
  }

  // the size of the list
  def length: Int = {
    // go (acc, [])     => acc
    // go (acc, _ :: t) => go(1 + acc, t)
    @tailrec
    def go(acc: Int, r: RList[T]): Int = r match {
      case RNil   => acc
      case _ :: t => go(1 + acc, t)
    }
//    go(0, this)

    foldLeft(0)((acc, _) => 1 + acc)
  }

  // Not tail recursive!!
  def reverse2: RList[T] = {
    @tailrec
    def go(r: RList[T], acc: RList[T] => RList[T]): RList[T] = r match {
      case RNil => acc(RNil)
      case h :: t => go(t, h :: acc(_))
    }
    go(this, identity)
  }

  // reverse the list
  def reverse: RList[T] = {
    @tailrec
    // two equations:
    // go(RNil  , acc) => acc
    // go(h :: t, acc) => go(t, h :: acc)
    def go(r: RList[T], acc: RList[T]): RList[T] = r match {
      case RNil   =>            acc
      case h :: t => go(t, h :: acc)
    }
//    go(this, RNil)
    // go(1 :: 2 :: 3 :: RNil,   RNil) ==
    // go(2 :: 3 :: RNil,   1 :: RNil) ==
    // go(3 :: RNil,   2 :: 1 :: RNil) ==
    // go(RNil,   3 :: 2 :: 1 :: RNil) ==
    //            3 :: 2 :: 1 :: RNil

    foldLeft(RNil: RList[T])((acc, h) => h :: acc)
  }

  // complexity:
  // O(this.length) + O(reverse)
  // O(this.length) + O(this.length)
  // 2 * O(this.length)
  // O(this.length)
  // concatenate another list to this one
  def ++[S >: T](other: RList[S]): RList[S] = {
    // go(RNil, acc) => acc
    // go(h :: t, acc) => go(t, h :: acc)
    @tailrec
    def go(r: RList[S], acc: RList[S]): RList[S] = r match {
      case RNil   => acc
      case h :: t => go(t, h :: acc)
    }

//    go(this.reverse, other)

    this.reverse.foldLeft(other)((acc, h) => h :: acc)

  }

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T] = {
    // go(_,     acc,     []) = acc.reverse
    // go(0,     acc, _ :: t) = acc.reverse ++ t
    // go(k + 1, acc, h :: t) = go(k, h :: acc, t)

    // go(2, [], [a,b,c,d]) =
    // go(1, [a], [b, c, d]) =
    // go(0, [b, a], [c, d]) =
    // [b, a].reverse ++ [d] =
    @tailrec
    def go(i: Int, acc: RList[T], rem: RList[T]): RList[T] = rem match {
      case RNil             => acc.reverse
      case _ :: t if i <= 0 => acc.reverse ++ t
      case h :: t           => go(i - 1, h :: acc, t)
    }
    go(index, RNil, this)
  }

  // the big 3
  // Complexity:
  // O(this.length) * O(f)
  def map[S](f: T => S): RList[S] = {
    @tailrec
    def map_(rem: RList[T], acc: RList[S]): RList[S] = rem match {
      case RNil   => acc.reverse
      case h :: t => map_(t, f(h) :: acc)
    }
//    map_(this, RNil)
    this.reverse.foldLeft(RNil: RList[S])((acc, h) => f(h) :: acc)
  }

  // Complexity:
  // O(Z^2)
  def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMap_(rem: RList[T], acc: RList[S]): RList[S] = rem match
      case RNil => acc.reverse
      case h :: t => flatMap_(t, f(h) ++ acc)

    @tailrec
    def concatenateAll(xss: RList[RList[S]], cur: RList[S], acc: RList[S]): RList[S] =
      (xss, cur) match
        case (RNil, RNil)     => acc
        case (hh :: tt, RNil) => concatenateAll(tt, hh, acc)
        case (_, h :: t)      => concatenateAll(xss, t, h :: acc)

    @tailrec
    def flatMap1_(rem: RList[T], acc: RList[RList[S]]): RList[S] = rem match
      case RNil => concatenateAll(acc, RNil, RNil)
      case h :: t => flatMap1_(t, f(h).reverse :: acc)

//    flatMap_(this, RNil)
//    this.reverse.foldLeft(RNil: RList[S])((acc, h) => f(h) ++ acc)
    flatMap1_(this, RNil)
  }

  def filter(pred: T => Boolean): RList[T] = {
    @tailrec
    def go(acc: RList[T], rem: RList[T]): RList[T] = rem match {
      case RNil              => acc.reverse
      case h :: t if pred(h) => go(h :: acc, t)
      case _ :: t            => go(     acc, t)
    }
    go(RNil, this)
  }

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  def rle: RList[(T, Int)] = {

    @tailrec
    def go(rem: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] = (rem, acc) match {
      case (RNil  , _               )            => acc.reverse
      case (h :: t, (h0, n) :: acctT) if h == h0 => go(t, (h, n + 1) :: acctT)
      case (h :: t, _               )            => go(t, (h,     1) :: acc  )
    }

    go(this, RNil)
  }

  // duplicate
  def duplicateEach0(k: Int): RList[T] = {
    def generate(k: Int)(t: T): RList[T] = from(Iterable.fill(k)(t))
    flatMap(generate(k))
  }

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T] = {
    // equations
    // go(3, h :: t, acc) => go(2, h :: t, h :: acc)
    // go(2, h :: t, acc) => go(1, h :: t, h :: acc)
    // go(1, h :: t, acc) => go(0, h :: t, h :: acc)
    // go(0, h :: t, acc) => go(k, t, acc)
    // go(_, [],     acc) => acc.reverse

    @tailrec
    def go(n: Int, rem: RList[T], acc: RList[T]): RList[T] = (n, rem) match {
      case (_, RNil  ) => acc.reverse
      case (0, _ :: t) => go(k, t, acc)
      case (_, h :: _) => go(n - 1, rem, h :: acc)
    }

    go(k, this, RNil)
  }

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T] = {
    // go(0, rem,  acc) => rem ++ acc.reverse
    // go(_, [],  acc)  => acc.reverse
    // go(k + 1, h :: t,  acc)  => go(k, t, h :: acc)
    @tailrec
    def go(i: Int, rem: RList[T], acc: RList[T]): RList[T] =
      (i, rem) match {
        case (0, RNil  ) => this
        case (0, _     ) => rem ++ acc.reverse
        case (_, RNil  ) => acc.reverse
        case (_, h :: t) => go(i - 1, t, h :: acc)
      }
    go(k % length, this, RNil)
  }

  // random sample
  def sample(k: Int): RList[T] = {
   @tailrec
   def go(i: Int, acc: RList[T]): RList[T] =
     if i <= 0 || this.isEmpty
     then
       acc
     else
      go(i - 1, this.apply(Random.nextInt(this.length)) :: acc)

    go(k, RNil)

    RList.from(List.fill(k)(this.apply(Random.nextInt(this.length))))
    RList.from((1 to k).map(_ => Random.nextInt(this.length)).map(apply))
  }


  def insertSort[S >: T: Ordering]: RList[S] = {
    import math.Ordering.Implicits.infixOrderingOps

    // ------------------------
    // Naive recursive function
    // ------------------------

    // insert1NonTC(s, Nil)    => s :: Nil
    // insert1NonTC(s, h :: t) => s :: h :: t               // s < h
    // insert1NonTC(s, h :: t) => h :: insert1NonTC(s, t)   // s >= h

    def insert1NonTC(s: S, lst: RList[S]): RList[S] =
      if lst.isEmpty || s < lst.head
      then s :: lst // this allows us to identify the type of the continuation
      else
        lst.head :: insert1NonTC(s, lst.tail)
    // lst.head :: lst.tail.head :: s :: lst.tail.tail


    // --------------------------------
    // Standard tail-recursive version
    // --------------------------------

    @tailrec
    def insert1(s: S, rest: RList[S], acc: RList[S]): RList[S] = // (1)
      if rest.isEmpty || s < rest.head
      then
        acc.reverse ++ (s :: rest)                              // (3)
      else
        insert1(s, rest.tail, rest.head :: acc)                  // (2)

    // (1) -> (2), (2), ... -> (3)


    // -----------------------
    // CPS transformation:
    // -----------------------
    type Cont[S] = RList[S] => RList[S]

    // this will be the evaluation function of the "Next" case
    def buildCont[S](lst: RList[S], cont: Cont[S]): Cont[S] =
      acc => cont(lst.head :: acc)

    @tailrec
    def insert1NonTC_1[S: Ordering](s: S, rest: RList[S], k: Cont[S]): RList[S] =
      if rest.isEmpty || s < rest.head
      then
        k(s :: rest)
      else
        insert1NonTC_1(s, rest.tail, k compose (acc => rest.head :: acc))


    // --------------------------------
    // Defunctionalize the CPS version
    // --------------------------------
//    enum Kont[S]:
//      case Next(h: S, next: Kont[S])
//
//    @tailrec
//    def insert1Def[S: Ordering](s: S, rest: RList[S], k: Kont[S]): RList[S] =
//      if rest.isEmpty || s < rest.head
//      then k match
//        case Kont.Next(h, next) => ???
//      else
//        insert1Def(s, rest.tail, Kont.Next(rest.head, k))



//    @tailrec
//    def sortAll(ts: RList[T], acc: RList[S]): RList[S] = ts match
//      case RNil   => acc
//      case h :: t => sortAll(t, sort1(RNil, h, acc))

//    sortAll(this, RNil)
//    foldLeft(RNil : RList[S])((acc, h) => sort1(RNil, h, acc))
//    foldLeft(RNil : RList[S])((acc, h) => insert1NonTC(h, acc))
    println("sorting: " + this)
    foldLeft(RNil : RList[S])((acc, h) => {
      println("fold: ");
//      insert1Def(h, acc, Kont.Done(RNil))
      insert1NonTC_1(h, acc, x => {println(x); x})
    })
  }


  def sorted2[S >: T: Ordering]: RList[S] = {
    import math.Ordering.Implicits.infixOrderingOps

    var ts = this
    var result: RList[S] = RNil
    while !ts.isEmpty do
      var before: RList[S] = RNil
      var after: RList[S] = result
      var done = false
      while !done do
        if after.isEmpty || after.head > ts.head then
          done = true
          result = (ts.head :: before).reverse ++ after
        else
          before = after.head :: before
          after = after.tail
      ts = ts.tail
    result
  }


  def splitAt(n: Int): (RList[T], RList[T]) = {
    @tailrec
    def split_(rest: RList[T], n: Int, acc: RList[T]): (RList[T], RList[T]) =
      if n <= 0 || rest.isEmpty
      then (acc.reverse, rest)
      else split_(rest.tail, n - 1, rest.head :: acc)

    split_(this, n, RNil)
  }

  /**
    * - split the list in half
    * - sort each half recursively
    * - merge sorted halves
    */
  def mergeSort1[S >: T: Ordering]: RList[S] = {
    import math.Ordering.Implicits.infixOrderingOps

    @tailrec
    def mergeTR(l: RList[S], r: RList[S], acc: RList[S]): RList[S] = (l, r) match
      case (a :: tl, b :: tr) => if a <= b then mergeTR(tl, r, a :: acc) else mergeTR(l, tr, b :: acc)
      case (l, RNil)          => acc.reverse ++ l
      case (RNil, r)          => acc.reverse ++ r

    if length < 2 then this
    else
      val (left, right) = splitAt(length / 2)
      mergeTR(left.mergeSort1, right.mergeSort1, RNil)
  }


  def mergeSort[S >: T: Ordering]: RList[S] = {
    import math.Ordering.Implicits.infixOrderingOps

    @tailrec
    def mergeTR(l: RList[S], r: RList[S], acc: RList[S]): RList[S] = (l, r) match
      case (a :: tl, b :: tr) => if a <= b then mergeTR(tl, r, a :: acc) else mergeTR(l, tr, b :: acc)
      case (l, RNil)          => acc.reverse ++ l
      case (RNil, r)          => acc.reverse ++ r

    def mergeSort_(lst: RList[S], k: RList[S] => RList[S]): RList[S] =
      if lst.length < 2 then
        k(lst)
      else
        val (l, r) = lst.splitAt(lst.length / 2)
        mergeSort_(l, al => mergeSort_(r, ar => k(mergeTR(al, ar, RNil))))

    mergeSort_(this, identity)
  }






}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(i: Int): Nothing = throw new NoSuchElementException

}

case class ::[+T](
  override val head: T,
  override val tail: RList[T]
) extends RList[T] {

  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(i: Int): T = {
    @tailrec
    def go(l: RList[T], j: Int): T = {
      if (j <= 0) l.head
      else go(l.tail, j - 1)
    }
    go(this, i)
  }
}

object RList {
  def from[T](it: Iterable[T]): RList[T] =
    it.foldRight[RList[T]](RNil)(_ :: _)

}

object ListProblems extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions() = {
    // test get-kth
    println(aSmallList.apply(0))
    println(aSmallList.apply(2))
    println(aLargeList.apply(8735))

    // test length
    println(aSmallList.length)
    println(aLargeList.length)

    // test reverse
    println(aSmallList.reverse)
    println(aLargeList.reverse)

    // test concat
    println(aSmallList ++ aLargeList)

    // test removeAt
    println(aLargeList.removeAt(13))

    // map
    println(aLargeList.map(x => 2 * x))
    // flatMap
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 1.3 seconds!
    println(System.currentTimeMillis() - time)
    // filter
    println(aLargeList.filter(x => x % 2 == 0))
  }

  /**
    * Medium difficulty functions
    */
  def testMediumDifficultyFunctions() = {
    // run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)
    // duplicateEach
    println(aSmallList.duplicateEach(4))
    // rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

  }

//  testMediumDifficultyFunctions()
//  testEasyFunctions()
//  println(aLargeList.sample(1000))
//  println(aSmallList.foldLeft(1)( _ * _))
//  println(aSmallList.length)
//  println(aSmallList.reverse)
//  println(aSmallList ++ aSmallList)
//  println(aSmallList.map(_ + 1))
//  println(aSmallList.flatMap(h => h :: (h + 1) :: RNil))
  val unorderedLst: RList[Int] = 2 :: 1 :: 5 :: 3 :: 0 :: RNil
//  println( unorderedLst.insertSort )
//  println( unorderedLst.sorted2 )
//  println(unorderedLst.splitAt(0))
//  println((2 :: RNil).mergeSort)
//  println((2 :: 1:: RNil).mergeSort)
  println(unorderedLst.mergeSort)

//  import math.Ordering.Implicits.infixOrderingOps
//  @tailrec
//  def insert1NonTC_1[S: Ordering](s: S, lst: RList[S], cont: RList[S] => RList[S]): RList[S] =
//    if (lst.isEmpty || s < lst.head)
//    then cont(s :: lst)
//    else insert1NonTC_1(s, lst.tail, lst.head :: _)

//  println(insert1NonTC_1(2, unorderedLst, identity))

}
