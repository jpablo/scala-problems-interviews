package puzzles.leetCode

// 121
// https://leetcode.com/problems/best-time-to-buy-and-sell-stock/


// You are given an array prices where prices[i] is the price of a given stock on the ith day.
// You want to maximize your profit by choosing a single day to buy one stock and choosing a *different* day in the future to sell that stock.
// Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.

/* 
Ideas:
  - two values: minPrices / maxProfit
  - alternate update
*/

type Price = Int

def maxProfit(prices: Array[Price]): Price = {
  var minPrice = Int.MaxValue
  var maxProfit = 0
  for (p <- prices) {
    val profit = p - minPrice
    if (profit < 0)
      minPrice = p
    else if (profit > maxProfit)
      maxProfit = profit
  }
  maxProfit
}

def maxProfit2(prices: Array[Price]): Price =
  prices.foldLeft((Int.MaxValue, 0)) { case (acc @ (minPrice, maxProfit), p) =>
    val profit = p - minPrice
    if (profit < 0)
      (p, maxProfit)
    else if (profit > maxProfit)
      (minPrice, profit)
    else
      acc
  }._2


@main def run121 =
  println(maxProfit2(Array(7,1,5,3,6,4)))
  // println(maxProfit2(Array(7,6,4,3,1)))
  // println(maxProfit2(Array(3,2,6,5,0,3)))
  // println(maxProfit2(Array(2,1,2,1,0,1,2)))