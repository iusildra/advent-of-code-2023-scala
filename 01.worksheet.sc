import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
val source = Files
  .readAllLines:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day1.txt"
  .asScala

/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def part1 =
  source
    .map: line =>
      val first = line.find(_.isDigit).get.asDigit
      val last = line.findLast(_.isDigit).get.asDigit
      first * 10 + last
    .sum

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
trait Entry:
  def firstIdx: Int
  def lastIdx: Int
case class IntEntry(line: String, firstIdx: Int, lastIdx: Int) extends Entry:
  def toInt(first: Boolean): Int =
    if first then line(firstIdx).asDigit
    else line(lastIdx).asDigit
case class StrEntry(entry: String, firstIdx: Int, lastIdx: Int) extends Entry:
  def toInt: Int = StrEntry.strDigits(entry)
object StrEntry:
  val strDigits = Map(
    "one" -> 1, "two" -> 2, "three" -> 3,
    "four" -> 4, "five" -> 5, "six" -> 6,
    "seven" -> 7, "eight" -> 8, "nine" -> 9
  )

// For a given line, get the first and last int digit (1, 2, ..., 9) indexes
def intDigitBoundedIndexes(line: String, p: Char => Boolean) =
  (line.indexWhere(p), line.lastIndexWhere(p)) match
    case (idx1, idx2) if idx1 != -1 => Some(IntEntry(line, idx1, idx2))
    case (_, _)                     => None

// For a given line and string digit (one, two, ... nine), return its first and last indexes (if they exist)
def strDigitsBoundedIndexes(line: String, entries: Set[String]) =
  object StrIndexBounds:
    def unapply(in: (String, String)): Option[(Int, Int)] =
      val firstIdx = in._1.indexOf(in._2)
      if firstIdx == -1 then None
      else Some(firstIdx, in._1.lastIndexOf(in._2))
  entries
    .map((line, _))
    .collect:
      case in @ StrIndexBounds(idx1, idx2) => StrEntry(in._2, idx1, idx2)

def part2 =
  source
    .map: line =>
      val digitIndexes =
        intDigitBoundedIndexes(line, _.isDigit)
          ++ strDigitsBoundedIndexes(line, StrEntry.strDigits.keySet)
      val firstDigit = digitIndexes.minBy(_.firstIdx) match
        case i: IntEntry => i.toInt(first = true)
        case s: StrEntry => s.toInt
      val lastDigit = digitIndexes.maxBy(_.lastIdx) match
        case i: IntEntry => i.toInt(first = false)
        case s: StrEntry => s.toInt
      firstDigit * 10 + lastDigit
    .sum
part2 // 52834
