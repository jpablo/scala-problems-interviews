package puzzles.leetCode.medium



// https://leetcode.com/problems/unique-paths/

// Hint: Use dynamic programming
// recursively: the count of each cell is the sum of counts to the right and bottom
// fill the values bottom to top, right to left

// a b . 
// c .
// .

// #a = #b + #c