package puzzles.leetCode


def sortedArrayToBST (nums: Array[Int]): TreeNode = {
  // Non-tail recursive version
  def go0 (l: Int, r: Int): TreeNode = {
    // 0 <= l <= r <= nums.length
    val mid = (l + r) / 2
    if (l > r || mid < 0 || mid >= nums.length) null
    else {      
      val left = go0(l, mid - 1)
      val right = go0(mid + 1, r)
      new TreeNode(nums(mid), left, right)              
    }
  }

  // [-10, -3, 0, 5, 9]
  //    0   1  2  3  4

  // [0,  1] -- [ 2 -> [3, 5] ]
  // [0, -1] -- [ 0 -> [1, 1], 2 -> [3, 5] ]
  // 


  // tail rec version
  // def go (l: Int, r: Int, stack: List[(Int, Int, Int)]): TreeNode = {
  //   // 0 <= l <= r <= nums.length
  //   val mid = (l + r) / 2
    
  //   if (l > r || mid < 0 || mid >= nums.length) 
  //     null
  //   else {      
  //     val left =  (l, mid - 1)
  //     val right = (mid + 1, r)
  //     new TreeNode(nums(mid), left, right)
  //     go(l, mid - 1, (nums(mid), mid + 1, r))
  //   }
  // }
  
  go0(0, nums.length)
}
