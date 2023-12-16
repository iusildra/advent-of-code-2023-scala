import scala.util.matching.Regex.Match
import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

val source = Files
  .readString:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day4.txt"

def part1: Unit =
  println(s"The solution is ${part1(source)}")

def part2: Unit =
  println(s"The solution is ${part2(source)}")

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
val intMatcher = """\d+""".r
def mapToIntSeq(xs: String) =
  intMatcher.findAllMatchIn(xs).map(_.matched.toInt).toSet
def extractor(line: String) =
  val Array(_, numbers) = line.split(":")
  val Array(winning, current) = numbers.split("\\|")
  (
    mapToIntSeq(winning),
    mapToIntSeq(current)
  )

/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def part1(input: String) =
  input
    .split("\n")
    .map: line =>
      val (winning, current) = extractor(line)
      val winningCount = current.filter(winning.contains).size
      if winningCount > 0 then Math.pow(2, winningCount - 1)
      else 0
    .sum

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
def part2(input: String) =
  val lines = input.split("\n")
  val count = Array.fill(lines.length)(1)
  lines.zipWithIndex.foreach: (line, i) =>
    val (winning, current) = extractor(line)
    val winningCount = current.filter(winning.contains).size
    for idx <- i + 1 to i + winningCount do count(idx) += count(i)
  count.sum

def test() =
  val range = 0 until 1000
  val t0 = System.nanoTime()
  for _ <- range
  do part2
  val t1 = System.nanoTime()
  (t1 - t0) / 1e6

test()
