import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.util.matching.Regex.Match
import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

val source = Files
  .readAllLines:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day5.txt"
  .asScala

def part1: Unit =
  println(s"The solution is ${part1(source)}")

def part2: Unit =
  println(s"The solution is ${part2(source)}")

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
case class Range(source: Long, length: Long)
object Range:
  def empty = Range(0, 0)
case class Mapping(origin: Range, destination: Range)

def seedParser(source: Buffer[String]) =
  val intMatcher = """\d+""".r
  intMatcher.findAllMatchIn(source(0)).map(_.matched.toLong).toArray

def seedParserV2(source: Buffer[String]) =
  val intPairMatcher = """\d+ \d+""".r
  intPairMatcher
    .findAllMatchIn(source(0))
    .map { res =>
      val Array(start, count) = res.matched.split(" ").map(_.toLong)
      Range(start, count)
    }

def infoParser(source: Buffer[String]) =
  source.tail
    .foldLeft(ListBuffer.empty[ListBuffer[Mapping]]) {
      case (acc, "") => acc.append(ListBuffer.empty[Mapping])
      case (acc, next) if next(0).isDigit =>
        val Array(src, dest, count) = next.split(" ").map(_.toLong)
        acc.last.addOne(Mapping(Range(dest, count), Range(src, count)))
        acc
      case (acc, _) => acc
    }
    .map(_.toList)
    .toList

def seedToLoc(seed: Long, mapping: List[Mapping]) =
  mapping
    .find: map =>
      map.origin.source <= seed
        && seed < map.origin.source + map.origin.length
    .map: map =>
      map.destination.source + (seed - map.origin.source)
    .getOrElse(seed)
/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def part1(input: Buffer[String]) =
  val infos = infoParser(input)
  seedParser(input)
    .map:
      infos.foldLeft(_) { seedToLoc }
    .min

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
case class Phase(min: Long, max: Long, mappings: List[Mapping])
def part2(input: Buffer[String]) =
  val seeds = seedParserV2(input)
  val infos = infoParser(input)
  seeds.foldLeft(Long.MaxValue): (globalMin, range) =>
    val newMin =
      (range.source to range.length).foldLeft(Long.MaxValue):
        (localMin, seed) =>
          val newLoc = infos.foldLeft(seed) { seedToLoc }
          if newLoc <= localMin then newLoc else localMin
    if newMin <= globalMin then newMin else globalMin

part2(source)

// Solution: 44187305