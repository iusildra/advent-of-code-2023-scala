import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.util.matching.Regex.Match
import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

val source = Files
  .readAllLines:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day8.txt"
  .asScala

def part1: Unit =
  println(s"The solution is ${part1(source)}")

def part2: Unit =
  println(s"The solution is ${part2(source)}")

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
case class Fork(left: String, right: String)

def directions(source: Buffer[String]) = source(0)
def forks(input: Buffer[String]) =
  input
    .drop(2)
    .map:
      case s"$name = ($left, $right)" => name -> Fork(left, right)
    .toMap

    
/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def computeDistance(
    start: String,
    directions: String,
    forks: Map[String, Fork],
    steps: Int = 0
): (String, Int) =
  if start == "ZZZ" then (start, steps)
  else if steps == directions.size then (start, steps)
  else if directions(steps) == 'L' then
    computeDistance(forks(start).left, directions, forks, steps + 1)
  else computeDistance(forks(start).right, directions, forks, steps + 1)
def lookForExit(
    starting: String,
    directions: String,
    forks: Map[String, Fork]
) =
  def loop(start: String, steps: Int = 0): Int =
    val result = computeDistance(start, directions, forks)
    if result._1 == "ZZZ" then steps + result._2
    else loop(result._1, steps + result._2)
  loop(starting)

def part1(input: Buffer[String]) =
  lookForExit("AAA", directions(input), forks(input))

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
def fetchStarts(input: Map[String, Fork]) =
  input.keySet.filter(_.endsWith("A")).toArray

def traverse(starts: Array[String], directions: String, forks: Map[String, Fork], index: Int= 0): (Array[String], Int) =
  if index == directions.size || starts.forall(_ endsWith "Z") then
    (starts, index)
  else
    val left = directions(index) == 'L'
    if left then
      traverse(starts.map(forks(_).left), directions, forks, index + 1)
    else
      traverse(starts.map(forks(_).right), directions, forks, index + 1)


def lookForExits(
    starting: Array[String],
    directions: String,
    forks: Map[String, Fork]
) =
  def loop(starts: Array[String], steps: Int = 0): Int =
    val left = directions(steps % directions.size) == 'L'
    val newStarts = traverse(starts, directions, forks)
    if steps % 307000 == 0 then println(s"${newStarts._1.filter(_ endsWith "Z").toSeq} <-- ${steps + newStarts._2}")
    if newStarts._1.forall(_ endsWith "Z") then steps + newStarts._2
    else loop(newStarts._1, steps + newStarts._2)
  loop(starting)

def part2(input: Buffer[String]) =
  val allForks = forks(input)
  lookForExits(fetchStarts(allForks), directions(input), allForks)

// val allForks = forks(source)
// fetchStarts(allForks).toSeq

def run1 = part1
def run2 = part2
