/*
Each title is listed for each word (omitting some minor words).
The titles are arranged so that the word being indexed is shown in a column on the page.
The position the lines have in the input file is shown on the left in the result.

Your solution should follow the following rules:
    The input is just a series of titles, one per line.
        Any leading or trailing spaces should be removed.
        Internal spaces should be retained (trimmed to one).
    A word is a maximal sequence of non-blank characters.
    The output line is at most 79 characters wide.
    The number is 4 characters wide, right-justified.
    There is a space after the number.
    The key word starts at position 40 (numbering from 1).
    If the part of the title left of the keyword is longer than 33, trim it (on the left) to 33.
    If the part of the keyword and the part to the right is longer than 40, trim it to 40.
    Each title appears in the output once for each word that isn't minor.
        Any article, conjunction and preposition is minor, e.g., and, to, and the words are minor words.
    If a title has a repeated word, it should be listed for each repetition.
    Sorting should be case-insensitive.
 */

import java.nio.file.Paths
import scala.collection.immutable.ListMap

val path = Paths.get("./kwic.txt").toAbsolutePath()
val forbidden_terms =
  List("to", "of", "the", "but", "for", "about", "over", "is", "and", "as", "a")

var lines = io.Source
  .fromFile(path.toString())
  .getLines()
  .toList
  .map((v) => v.trim().replaceAll("\\s{2,}", " "))

def normalize_title(t: String) = t.split(" ").toList
def short_term(t: String) =
  t.replaceAll("^[^a-zA-Z]", "").replaceAll("[^a-zA-Z]$", "").toLowerCase

def is_term_forbidden(t: String) = forbidden_terms.contains(t)
def nth_index_of(s: String, sub: String, i: Int): Int = i match {
  case 0 => s.indexOf(sub, 0)
  case x => s.indexOf(sub, nth_index_of(s, sub, i - 1) + 1)
}

def process_title(
    t: String,
    i: Int,
    kwic_map: Map[String, List[(Int, Int)]]
) =
  val norm = normalize_title(t)
  def inner(
      l: List[String],
      counts: Map[String, Int],
      k: Map[String, List[(Int, Int)]]
  ): Map[String, List[(Int, Int)]] = l match {
    case Nil => k
    case h :: tl =>
      val term = short_term(h)
      val idx = counts.getOrElse(term, 0)
      val existing = k.getOrElse(term, List())
      if is_term_forbidden(term) then inner(tl, counts, k)
      else
        inner(
          tl,
          counts + (term -> (idx + 1)),
          k + (term -> ((
            i,
            nth_index_of(t.toLowerCase, term, idx)
          ) :: existing))
        )
  }
  inner(norm, Map[String, Int](), kwic_map)

def print_title(title_idx: Int, term_idx: Int) =
  val title = lines.apply(title_idx - 1)
  var str_idx = ((" " * 4) + title_idx)
  str_idx = str_idx.substring(str_idx.length() - 4, str_idx.length())
  var str_before_term = ((" " * 40) + title)
    .substring(term_idx + 40 - 33, term_idx + 40)
  var str_after_term = (title + (" " * 40))
    .substring(term_idx, term_idx + 40)
  var str = str_idx + " " + str_before_term + str_after_term
  println(str)

def kwic() =
  def kwic_inner(
      l: List[(String, Int)],
      m: Map[String, List[(Int, Int)]]
  ): Map[String, List[(Int, Int)]] =
    l match {
      case Nil => m
      case h :: t =>
        h match {
          case (el, idx) =>
            kwic_inner(t, process_title(el, idx + 1, m))
        }
    }
  val kwic_map = kwic_inner(lines.zipWithIndex, Map[String, List[(Int, Int)]]())
  val map = ListMap(kwic_map.toSeq.sortWith(_._1 < _._1)*)
  map.foreachEntry((_, li) => li.foreach(v => print_title(v(0), v(1))))

@main def p() =
  kwic()
