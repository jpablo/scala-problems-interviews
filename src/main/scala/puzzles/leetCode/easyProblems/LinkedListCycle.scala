package puzzles.leetCode

def hasCycle(head: ListNode): Boolean = {
  var nodes = Set.empty[ListNode]
  var cycle = false
  var current = head
  while (current != null && !cycle) {
    if (nodes contains current)
      cycle = true
    else {
      nodes += current
      current = current.next
    }          
  }
  cycle        
}

// input:
// [3,2,0,-4]
// 1
