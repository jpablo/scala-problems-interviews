package puzzles;

import java.util.ArrayList;
import java.util.List;

class PalindromePartition1 {
  
  static public List<List<String>> partition(String s) {
    List<List<String>> result = new ArrayList<List<String>>();
    dfs(0, result, new ArrayList<String>(), s);
    return result;
  }

  static void dfs(int start, List<List<String>> result, List<String> currentList, String s) {
    if (start >= s.length())
      result.add(new ArrayList<String>(currentList));

    for (int end = start; end < s.length(); end++) {
      var ss = s.substring(start, end + 1);
      if (isPalindrome(s, start, end)) {
        // add current substring in the currentList
        currentList.add(ss);
        dfs(end + 1, result, currentList, s);
        // backtrack and remove the current substring from currentList
        currentList.remove(currentList.size() - 1);
      }
    }
  }

  static boolean isPalindrome(String s, int low, int high) {
    while (low < high) {
      if (s.charAt(low++) != s.charAt(high--))
        return false;
    }
    return true;
  }

  public static void main(String[] args) {
    System.out.println(PalindromePartition1.partition("aab"));
  }

}
