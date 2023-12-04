import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex.Match
import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
val source = Files
  .readAllLines:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day3.txt"
  .asScala

case class Coord(x: Int, y: Int):
  def within(start: Coord, end: Coord) =
    if y < start.y || y > end.y then false
    else if x < start.x || x > end.x then false
    else true
case class PartNumber(value: Int, start: Coord, end: Coord)
case class Symbol(sym: String, pos: Coord):
  def neighborOf(number: PartNumber) = pos.within(
    Coord(number.start.x - 1, number.start.y - 1),
    Coord(number.end.x + 1, number.end.y + 1)
  )

object IsInt:
  def unapply(in: Match): Option[Int] = in.matched.toIntOption

def findPartsAndSymbols() =
  val extractor = """(\d+)|[^.\d]""".r
  source.zipWithIndex.flatMap: (line, i) =>
    extractor
      .findAllMatchIn(line)
      .map:
        case m @ IsInt(nb) =>
          PartNumber(nb, Coord(m.start, i), Coord(m.end - 1, i))
        case s => Symbol(s.matched, Coord(s.start, i))
/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */

def part1 =
  val all = findPartsAndSymbols()
  val symbols = all.collect { case s: Symbol => s }
  all
    .collect:
      case n: PartNumber if symbols.exists(_.neighborOf(n)) =>
        n.value
    .sum

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
case class Gear(part: PartNumber, symbol: Symbol)

def part2 =
  val all = findPartsAndSymbols()
  val symbols = all.collect { case s: Symbol => s }
  all
    .flatMap:
      case n: PartNumber =>
        symbols
          .find(_.neighborOf(n))
          .filter(_.sym == "*")
          .map(Gear(n, _))
      case _ => None
    .groupMap(_.symbol)(_.part.value)
    .filter(_._2.length == 2)
    .foldLeft(0) { _ + _._2.product }

part1
part2