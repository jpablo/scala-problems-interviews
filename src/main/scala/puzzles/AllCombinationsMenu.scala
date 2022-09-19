package puzzles

import scala.collection.mutable.ArrayBuffer

// https://xkcd.com/287/

val menu = List[(String, Double)](
  "fruit" -> 2,
  "fries" -> 3,
  "salad" -> 4,
  "wings" -> 5,
  "sticks"-> 6,
  "plate" -> 7,
)

def comboPrice(lst: List[String]): Double =
  lst.map(menu.toMap).sum

// Find all menu combinations that add up to a given price
// strategy: divide and conquer
// strategy: reduce to simple cases

def combinations1(budget: Double): List[List[String]] =
  if budget <= 0 then 
    List(List.empty)
  else
    for
      (item, price) <- menu
      if price <= budget
      combination <- combinations1(budget - price)
    yield
      item :: combination



def combinations2(budget: Double): List[List[String]] =
  if budget <= 0 then
    List(List.empty)
  else
    menu
      .filter(_._2 <= budget)
      .flatMap { case (item, price) =>
        combinations2(budget - price).map(item :: _)
      }



@main def runCombinations() =

  for i <- 0 to 4 do
    println(s"$i -> ${combinations1(i)}")
  

  def testCases = List(
    // 0 -> List(List())
    // 1 -> List()
    // 2 -> List(List(fruit))
    // 3 -> List(List(fries))
    // 4 -> List(List(fruit, fruit), List(salad))
    // 5 -> List(List(fruit, fries), List(fries, fruit), List(wings))
    // 6 -> List(List(fruit, fruit, fruit), List(fruit, salad), List(fries, fries), List(salad, fruit), List(sticks))    
  )

  // for (price, expected) <- testCases do
  //   val result = combinations(price)
  //   assert(result.sorted == expected.sorted, s"$result != $expected")
