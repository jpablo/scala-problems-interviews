package puzzles.leetCode


/* 
The only way we can avoid using O(n)O extra space is by modifying the input in-place.

The strategy we can use is to reverse the second half of the Linked List in-place (modifying the Linked List structure), and then comparing it with the first half. Afterwards, we should re-reverse the second half and put the list back together. While you don't need to restore the list to pass the test cases, it is still good programming practice because the function could be a part of a bigger program that doesn't want the Linked List broken.

Algorithm

Specifically, the steps we need to do are:

Find the end of the first half.
Reverse the second half.
Determine whether or not there is a palindrome.
Restore the list.
Return the result.

 */

