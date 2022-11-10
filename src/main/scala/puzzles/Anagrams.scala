package puzzles

import scala.language.unsafeNulls

val dictionary = List(
  // ----
  "Cat",
  "Dog",
  // ----
  "Lin",
  "Linux",
  "nil",
  "null",
  "Rex",
  "rulez",
  "Uzi",
  "Zulu",
)

// 1. Combine words and create count of characters
// val counts: Map[Char, Int] = Map('L' -> 1, 'u' -> 2, ...)
// counts is a "budget": val budget0 = counts
// take a word `w` and calculate the cost: `cost(w)`
//    ("Lin", Map('L' -> 1, 'i' -> 1, 'n' -> 1))
// new budget1 = budget0 - cost(w)
// recursive step:
//  generate new words for new budget
//  val wordsPrev = sentenceAnagrams(dictionary, budget1)
// combine w with wordsPrev: w :: wordsPrev

type Count  = Int
type Word = String
type Sentence = List[Word]
type Budget = Map[Char, Count]


extension (budget: Budget)

  def budgetToString: String =
    budget.toList.map((k,v) => s"$k:$v").sorted.mkString("[", ", ", "]")

  // def nonZero: Boolean = 
  //   budget.values.forall(_ >= 0)

  def isZero: Boolean = 
    budget.values.forall(_ == 0)

  def someNegative: Boolean = 
    budget.values.exists(_ < 0)

  def subtract(other: Budget): Budget =
    val keys1 = budget.keySet
    val keys2 = other.keySet
    val diff = 
      for
        k <- keys1 ++ keys2
        c1 = budget.getOrElse(k, 0)
        c2 = other.getOrElse(k, 0)
      yield
        (k, c1 - c2)
    diff.toMap


def calculateCost (word: Word): Budget = 
  word.toLowerCase.groupBy(identity).transform((_, s) => s.length)



def sentenceAnagrams (dictionary: List[Word], totalBudget: Budget): List[Sentence] =
  val sentences: List[Sentence] = 
    for 
      word <- dictionary 
      sentence <-
        val newBudget = totalBudget.subtract(calculateCost(word))
        if newBudget.someNegative then
          List(List.empty)
        else if newBudget.isZero then 
          List(List(word))
        else
          val prevSentences = sentenceAnagrams(dictionary, newBudget)
          prevSentences.map(word :: _)
    yield
      sentence
  sentences.filter(_.nonEmpty)



def sentenceAnagramsTR (
  dictionary: List[Word], 
  incomplete: List[(Budget, Sentence)], 
  complete  : List[Sentence] = List.empty, 
): List[Sentence] =
  println(incomplete)
  if incomplete.isEmpty then
    complete
  else
    val sentences = 
      for 
        word <- dictionary
        (totalBudget, sentence) <- incomplete
        newSentence = word :: sentence
        newBudget = totalBudget.subtract(calculateCost(newSentence.mkString))
      yield
        (newBudget, newSentence)

    val groups = 
      sentences.groupBy { (budget, sentence) => 
        if budget.someNegative then "discard" 
        else if budget.isZero then "complete"
        else "incomplete"
      }
    
    sentenceAnagramsTR(dictionary, 
      groups.getOrElse("incomplete", List.empty), 
      groups.getOrElse("complete", List.empty).map(_._2) ++ complete
    )


val sentence = List("Linux", "rulez")

val expected = List(
  List("Lin", "Rex", "Zulu"),
  List("Lin", "Zulu", "Rex"),

  List("Linux", "rulez"),

  List("Rex", "Lin", "Zulu"),
  List("Rex", "Uzi", "null"),
  List("Rex", "Zulu", "Lin"),
  List("Rex", "Zulu", "nil"),
  List("Rex", "nil", "Zulu"),
  List("Rex", "null", "Uzi"),

  List("Uzi", "Rex", "null"),
  List("Uzi", "null", "Rex"),

  List("Zulu", "Lin", "Rex"),
  List("Zulu", "Rex", "Lin"),
  List("Zulu", "Rex", "nil"),
  List("Zulu", "nil", "Rex"),
  
  List("nil", "Rex", "Zulu"),
  List("nil", "Zulu", "Rex"),
  List("null", "Rex", "Uzi"),
  List("null", "Uzi", "Rex"),

  List("rulez", "Linux"),
)

@main def runAnagrams =
  
  // val b1 = calculateCost(sentence.mkString)
  // val b2 = calculateCost("Zulu")
  // println(b1)
  // println(b2)
  // val b3 = subtract(b1, b2)
  // println(b3)
  // println(allowed(b3))

  val budget = calculateCost(sentence.mkString)
  // val anas = sentenceAnagrams(dictionary, budget)
  val anas = sentenceAnagramsTR(dictionary, dictionary.map(w => (budget, List(w))))
  println("-------------------")
  anas.foreach(println)
  assert(anas.toSet == expected.toSet)

  println("Ok")

  val alphabet = sentence.flatten.distinct
  
  
  // ------------------------------------
  // 1. All combinations of a fixed length
  // ------------------------------------
  // n = 1
  // val n1 = for c <- alphabet yield c

  // n = 2
  // val n2 = 
  //   for 
  //     c1 <- alphabet 
  //     c2 <- alphabet 
  //   yield
  //     (c1, c2)
    
  // ---------------------------------------
  // 2. All combinations of arbitrary length
  // ---------------------------------------
  def combinations (n: Int): List[List[Char]] =
    if n == 0 then 
      List(List.empty)
    else
      val prev = combinations(n - 1)
      val next = 
        for
          c <- alphabet
          word <- prev
        yield
          c :: word
      next

  // println(combinations(1))

  // tail recursive version
  // 1. move result of recursive step as a parameter (accumulator)
  // 2. pass current result as accumulator argument of recursive call
  // 3. base case: return accumulator
  // 4. Initialize accumulator to non-rec base case
  def combinationsTR (n: Int, prev: List[List[Char]] = List(List.empty)): List[List[Char]] =
    if n == 0 then 
      prev
    else
      combinationsTR(n - 1, 
        for c <- alphabet; word <- prev yield c :: word
      )


  // combinationsTR(3).zip(combinations(3)).foreach((l, r) => println(s"${l.mkString}, ${r.mkString}"))
  // combinationsTR(2).foreach(w => println(w.mkString))

  // dictionary.foreach(println)

