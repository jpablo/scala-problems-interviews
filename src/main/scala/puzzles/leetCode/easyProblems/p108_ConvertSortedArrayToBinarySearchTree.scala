package puzzles.leetCode

// 108
// https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/



/* 
  Given an integer array nums where the elements are sorted in ascending order, 
  convert it to a height-balanced binary search tree.
*/

/* 

Ideas:

- root of the tree: near midpoint
- split array: left ++ [root] ++ right

equations:
  toBST [a] = TreeNode(a)
  toBST [a, b, c] = TreeNode(b, TreeNode(a), TreeNode(c))
  toBST left ++ [m] ++ right = TreeNode(m, toBST left, toBST right)
 */

def sortedArrayToBST (nums: Array[Int]): TreeNode | Null = {
  // Non-tail recursive version
  def loop (l: Int, r: Int): TreeNode | Null = {
    // 0 <= l <= r <= nums.length
    val mid = (l + r) / 2
    if (l > r || mid < 0 || mid >= nums.length) null
    else {      
      val left  = loop(l, mid - 1)
      val right = loop(mid + 1, r)
      new TreeNode(nums(mid), left, right)              
    }
  }

  
  loop(0, nums.length)
}
