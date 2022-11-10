package puzzles.leetCode

type Price = Int

def maxProfit(prices: Array[Price]): Price = {
  var minPrice = Int.MaxValue
  var maxProfit = 0
  var i = 0
  while (i < prices.length) {
    if (prices(i) < minPrice)
      minPrice = prices(i)
    else if (prices(i) - minPrice > maxProfit)
      maxProfit = prices(i) - minPrice
    i += 1
  }
  maxProfit
}

def maxProfit2(prices: Array[Price], minPrice: Int = Int.MaxValue, maxProfit: Int = 0, i: Int = 0): Price =
  if i == prices.length then maxProfit
  else 
    val profit = prices(i) - minPrice
         if profit < 0         then maxProfit2(prices, prices(i), maxProfit, i + 1)
    else if profit > maxProfit then maxProfit2(prices, minPrice , profit   , i + 1)
    else                            maxProfit2(prices, minPrice , maxProfit, i + 1)



@main def runMaxProfit =
  // println(maxProfit(Array(7,1,5,3,6,4)))
  // println(maxProfit(Array(7,6,4,3,1)))
  // println(maxProfit(Array(3,2,6,5,0,3)))
  println(maxProfit2(Array(3,2,6,5,0,3)))
  // println(maxProfit(Array(2,1,2,1,0,1,2)))