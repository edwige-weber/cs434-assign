package forcomp

import common._

import scala.annotation.tailrec

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    @tailrec
    def wordOccurrencesAux(w : Word, occurrences_list : List[(Char, Int)]) : Occurrences = {
      if(w.isEmpty) occurrences_list.reverse
      else {
        val occurrences = list_contains_element(occurrences_list, w.head)
        if( occurrences == -1) wordOccurrencesAux(w.tail, (w.head, 1) :: occurrences_list)
        else {
          wordOccurrencesAux(w.tail,modify_list(occurrences_list, (w.head, 1)))
        }
      }
    }

    wordOccurrencesAux(w.toLowerCase(), List())
  }

  @tailrec
  private def list_contains_element(list : List[(Char, Int)], element : Char) : Int = {
    list match {
      case Nil => -1
      case (char, occurrence) :: _ if char == element => occurrence
      case _ :: tail => list_contains_element(tail, element)
    }
  }

  private def modify_list(list: List[(Char, Int)], element: (Char, Int) ) : List[(Char, Int)] = {
    list match {
      case Nil => Nil
      case (char, int) :: tail if char == element._1 => (char, int+element._2) :: modify_list(tail, element)
      case head :: tail => head :: modify_list(tail, element)
    }
  }
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = sentenceOccurrencesAux(s, List())

  @tailrec
  def sentenceOccurrencesAux(s : Sentence, sentence_occurrences : List[(Char, Int)]) : List[(Char, Int)] = {
    @tailrec
    def concat_list(l1 : List[(Char, Int)], l2 : List[(Char, Int)]) : List[(Char, Int)] = {
      l2 match {
        case Nil => l1
        case l2_head :: l2_tail if list_contains_element(l1, l2_head._1) > 0 => concat_list(modify_list(l1, l2_head), l2_tail)
        case l2_head :: l2_tail => concat_list(l2_head::l1, l2_tail)
      }
    }

    s match {
      case Nil => sentence_occurrences.reverse
      case head :: tail => sentenceOccurrencesAux(tail, concat_list(sentence_occurrences, wordOccurrences(head)))
    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((element : Word) => wordOccurrences(element))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val word_occurrences = wordOccurrences(word)
    dictionaryByOccurrences.getOrElse(word_occurrences, word::List())
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case head :: tail =>
      for {
        i <- combinations(tail)
        j <- 0 until head._2 + 1
      }
      yield {
        if (j == 0) i
        else (head._1, j) :: i
      }
    case _ => List(List())
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  @tailrec
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractAux(list : Occurrences, element : (Char, Int)) : Occurrences = list match {
      case head :: tail if head._1 == element._1 =>
        if(head._2 > element._2){
          (head._1, head._2 - element._2) :: subtractAux(tail, element)
        } else subtractAux(tail, element)
      case head :: tail => head :: subtractAux(tail, element)
      case _ => x
    }

    y match {
      case head :: tail => subtract(subtractAux(x, head), tail)
      case _ => x
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagramsAux (occurrences : Occurrences) : List[Sentence] = {
      if(occurrences.isEmpty) List(List())
      else {
        for {
          combination <- combinations(occurrences)
          anagram <- dictionaryByOccurrences.get(combination) match {
            case Some(element) => element
            case _ => List()
          }
          res <- sentenceAnagramsAux(subtract(occurrences, combination))
        }
          yield {
            anagram :: res
          }
      }
    }

    sentenceAnagramsAux(sentenceOccurrences(sentence))
  }

}
