import scala.collection.immutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.util.matching.Regex.Match
import java.nio.file.Path
import java.nio.file.Files

val source = Files
  .readString:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day16.txt"

def part1: Unit =
  println(s"The solution is ${part1(source)}")

def part2: Unit =
  println(s"The solution is ${part2(source)}")

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
enum Direction:
  case Up, Right, Down, Left
type MirrorDirection = Direction
type SplitterDirection = Direction | (Direction, Direction)

case class Coord(x: Int, y: Int)

enum Symbol:
  case Slash, Backslash, Pipe, Dash
object Symbol:
  def fromChar(char: Char) =
    char match
      case '/'  => Slash
      case '\\' => Backslash
      case '|'  => Pipe
      case '-'  => Dash

sealed abstract class Element[T]:
  val pos: Coord
  def name: Char
  def nextDirection(comingFrom: Direction): T
  def pathTo(other: Element[?]): Seq[Coord] =
    pathTo(other.pos)
  def pathTo(coord: Coord): Seq[Coord] =
    if pos.x == coord.x then
      if pos.y < coord.y then (pos.y to coord.y).map(Coord(pos.x, _))
      else (coord.y to pos.y).map(Coord(pos.x, _))
    else if (pos.x < coord.x) then (pos.x to coord.x).map(Coord(_, pos.y))
    else (coord.x to pos.x).map(Coord(_, pos.y))

abstract class Mirror(symbol: Symbol, pos: Coord)
    extends Element[MirrorDirection]
case class SlashMirror(pos: Coord) extends Mirror(Symbol.Slash, pos):
  def name: Char = '/'
  def nextDirection(goingTo: Direction): MirrorDirection =
    goingTo match
      case Direction.Up    => Direction.Right
      case Direction.Left  => Direction.Down
      case Direction.Right => Direction.Up
      case Direction.Down  => Direction.Left

case class BackslashMirror(pos: Coord) extends Mirror(Symbol.Backslash, pos):
  def name: Char = '\\'
  def nextDirection(goingTo: Direction): MirrorDirection =
    goingTo match
      case Direction.Up    => Direction.Left
      case Direction.Right => Direction.Down
      case Direction.Down  => Direction.Right
      case Direction.Left  => Direction.Up

object Mirror:
  def apply(sym: Char, x: Int, y: Int) =
    Symbol.fromChar(sym) match
      case Symbol.Backslash => BackslashMirror(Coord(x, y))
      case Symbol.Slash     => SlashMirror(Coord(x, y))
      case _                => throw new IllegalArgumentException

abstract class Splitter(pos: Coord) extends Element[SplitterDirection]
case class VSplitter(pos: Coord) extends Splitter(pos):
  def name: Char = '|'
  def nextDirection(goingTo: Direction): SplitterDirection =
    goingTo match
      case d @ (Direction.Up | Direction.Down) => d
      case _                                   => (Direction.Up, Direction.Down)
case class HSplitter(pos: Coord) extends Splitter(pos):
  def name: Char = '-'
  def nextDirection(comingFrom: Direction): SplitterDirection =
    comingFrom match
      case d @ (Direction.Left | Direction.Left) => d
      case _ => (Direction.Left, Direction.Right)

object Splitter:
  def apply(sym: Char, x: Int, y: Int) =
    Symbol.fromChar(sym) match
      case Symbol.Pipe => VSplitter(Coord(x, y))
      case Symbol.Dash => HSplitter(Coord(x, y))
      case _           => throw new IllegalArgumentException

case class Empty(pos: Coord) extends Element[Nothing]:
  def name: Char = '.'
  def nextDirection(comingFrom: Direction): Nothing =
    throw new UnsupportedOperationException

def findElements(source: Array[String]): Seq[Seq[Element[?]]] =
  source.zipWithIndex
    .map: (line, y) =>
      line.zipWithIndex
        .filter(_._1 != '.')
        .map: (sym, x) =>
          sym match
            case '/' | '\\' => Mirror(sym, x, y)
            case '-' | '|'  => Splitter(sym, x, y)
        .toSeq
    .toSeq

def solution(input: Array[String], origin: Coord, dir: Direction) =
  val elements = findElements(input)
  val elementsByRow = elements.flatten.groupBy(_.pos.y)
  val elementsByColumn = elements.flatten.groupBy(_.pos.x)
  def minY(x: Int) = elementsByColumn(x)(0).pos.y
  def maxY(x: Int) = elementsByColumn(x).last.pos.y
  def minX(y: Int) = elementsByRow(y)(0).pos.x
  def maxX(y: Int) = elementsByRow(y).last.pos.x
  val activated = Array.fill(input.length)(Array.fill(input(0).length())(false))
  var memo = Set.empty[(Coord, Coord)]
  def findNext(
      elem: Element[?],
      goingTo: Direction
  ): Element[?] =
    goingTo match
      case Direction.Left if elem.pos.x > minX(elem.pos.y) =>
        val byRow = elementsByRow(elem.pos.y)
        byRow(byRow.indexOf(elem) - 1)
      case Direction.Left =>
        Empty(Coord(0, elem.pos.y))
      case Direction.Right if elem.pos.x < maxX(elem.pos.y) =>
        val byRow = elementsByRow(elem.pos.y)
        byRow(byRow.indexOf(elem) + 1)
      case Direction.Right =>
        Empty(Coord(input(0).length() - 1, elem.pos.y))
      case Direction.Up if elem.pos.y > minY(elem.pos.x) =>
        val byCol = elementsByColumn(elem.pos.x)
        byCol(byCol.indexOf(elem) - 1)
      case Direction.Up =>
        Empty(Coord(elem.pos.x, 0))
      case Direction.Down if elem.pos.y < maxY(elem.pos.x) =>
        val byCol = elementsByColumn(elem.pos.x)
        byCol(byCol.indexOf(elem) + 1)
      case Direction.Down =>
        Empty(Coord(elem.pos.x, input.length - 1))

  def activate(
      from: Element[?],
      to: Coord
  ) =
    from
      .pathTo(to)
      .foreach:
        case Coord(x, y) => activated(y)(x) = true

  def follow(
      current: Element[?],
      newDir: Direction,
      rest: Queue[(Element[?], Direction)]
  ) =
    val followup = findNext(current, newDir)
    if (memo.contains((current.pos, followup.pos))) then loop(rest)
    else
      memo += ((current.pos, followup.pos))
      activate(current, followup.pos)
      followup match
        case Empty(pos) => loop(rest)
        case next @ (_: Mirror | _: Splitter) =>
          loop(rest.enqueue((next, newDir)))

  def loop(elems: Queue[(Element[?], Direction)]): Unit =
    if elems.isEmpty then ()
    else
      elems.dequeue match
        case ((_: Empty, _), _) => throw new UnsupportedOperationException
        case ((m: Mirror, goingTo), rest) =>
          val newDir = m.nextDirection(goingTo)
          follow(m, newDir, rest)
        case ((s: Splitter, goingTo), rest) =>
          s.nextDirection(goingTo) match
            case d: Direction => follow(s, d, rest)
            case (d1: Direction, d2: Direction) =>
              val list = List(d1, d2)
              val queue = list.foldLeft(rest):(acc, dir) =>
                val followup = findNext(s, dir)
                if (memo.contains((s.pos, followup.pos))) then acc
                else
                  memo += ((s.pos, followup.pos))
                  activate(s, followup.pos)
                  followup match
                    case Empty(pos) => acc
                    case next @ (_: Mirror | _: Splitter) =>
                      acc.enqueue(next -> dir)
              loop(queue)
  val starting = dir match
    case Direction.Right => elementsByRow(origin.y)(0)
    case Direction.Down => elementsByColumn(origin.x)(0)
    case Direction.Left => elementsByRow(origin.y).last
    case Direction.Up => elementsByColumn(origin.x).last
  
  activate(starting, origin)
  loop(Queue(starting -> dir))

  // println(activated.zipWithIndex.map((line, i) => f"$i%03d " + line.map(if _ then '#' else '.').mkString).mkString("\n"))
  activated.flatten.count(identity)

/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def part1(input: String) =
  solution(input.split("\n"), Coord(0, 0), Direction.Right)

part1
/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
def part2(input: String) =
  val lines = input.split("\n")
  val left = (0 until lines.length).map(i => (Coord(0, i), Direction.Right))
  val right = (0 until lines.length).map(i =>
    (Coord(lines(0).length() - 1, i), Direction.Left)
  )
  val top = (0 until lines(0).length()).map(i => (Coord(i, 0), Direction.Down))
  val bottom = (0 until lines(0).length()).map(i =>
    (Coord(i, lines.length - 1), Direction.Up)
  )
  val borders = left ++ right ++ top ++ bottom
  borders.map((coord, dir) => solution(lines, coord, dir)).max

part2
