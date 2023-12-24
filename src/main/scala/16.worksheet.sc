import scala.annotation.tailrec
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

case class Coord(x: Int, y: Int)

sealed abstract class Element:
  val pos: Coord
  def nextDirection(comingFrom: Direction): List[Direction]
  def pathTo(coord: Coord): Seq[Coord] =
    if pos.x == coord.x then
      if pos.y < coord.y then (pos.y to coord.y).map(Coord(pos.x, _))
      else (coord.y to pos.y).map(Coord(pos.x, _))
    else if (pos.x < coord.x) then (pos.x to coord.x).map(Coord(_, pos.y))
    else (coord.x to pos.x).map(Coord(_, pos.y))

object Element:
  def apply(sym: Char, x: Int, y: Int) =
    sym match
      case '\\' => BackslashMirror(Coord(x, y))
      case '/'  => SlashMirror(Coord(x, y))
      case '|'  => VSplitter(Coord(x, y))
      case '-'  => HSplitter(Coord(x, y))
      case _    => throw new IllegalArgumentException

case class SlashMirror(override val pos: Coord) extends Element:
  def nextDirection(goingTo: Direction) =
    goingTo match
      case Direction.Up    => List(Direction.Right)
      case Direction.Left  => List(Direction.Down)
      case Direction.Right => List(Direction.Up)
      case Direction.Down  => List(Direction.Left)

case class BackslashMirror(override val pos: Coord) extends Element:
  def nextDirection(goingTo: Direction) =
    goingTo match
      case Direction.Up    => List(Direction.Left)
      case Direction.Right => List(Direction.Down)
      case Direction.Down  => List(Direction.Right)
      case Direction.Left  => List(Direction.Up)

case class VSplitter(pos: Coord) extends Element:
  def nextDirection(goingTo: Direction) =
    goingTo match
      case d @ (Direction.Up | Direction.Down) => List(d)
      case _ => List(Direction.Up, Direction.Down)
case class HSplitter(pos: Coord) extends Element:
  def nextDirection(comingFrom: Direction) =
    comingFrom match
      case d @ (Direction.Left | Direction.Left) => List(d)
      case _ => List(Direction.Left, Direction.Right)

case class Empty(pos: Coord) extends Element:
  def nextDirection(comingFrom: Direction): Nothing =
    throw new UnsupportedOperationException

def findElements(source: Array[String]) =
  source.zipWithIndex
    .map: (line, y) =>
      line.zipWithIndex
        .filter(_._1 != '.')
        .map { (sym, x) => Element(sym, x, y) }

def solution(input: Array[String], origin: Coord, dir: Direction) =
  val elements = findElements(input)
  val elementsByRow = elements.flatten.groupBy(_.pos.y)
  val elementsByColumn = elements.flatten.groupBy(_.pos.x)
  val minY = elementsByColumn.map((k, v) => (k, v(0).pos.y))
  val maxY = elementsByColumn.map((k, v) => (k, v.last.pos.y))
  val minX = elementsByRow.map((k, v) => (k, v(0).pos.x))
  val maxX = elementsByRow.map((k, v) => (k, v.last.pos.x))
  val activated = Array.fill(input.length)(Array.fill(input(0).length())(false))
  def findNext(
      elem: Element,
      goingTo: Direction
  ): Element =
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

  def activate(from: Element, to: Coord) =
    from
      .pathTo(to)
      .foreach:
        case Coord(x, y) => activated(y)(x) = true

  @tailrec
  def loop(
      elems: Queue[(Element, Direction)],
      memo: Set[(Coord, Coord)]
  ): Unit =
    if elems.isEmpty then ()
    else
      elems.dequeue match
        case ((_: Empty, _), _) => throw new UnsupportedOperationException
        case ((elem, goingTo), rest) =>
          val nextElems =
            elem
              .nextDirection(goingTo)
              .foldLeft((rest, memo)): (acc, dir) =>
                val followup = findNext(elem, dir)
                if (memo.contains((elem.pos, followup.pos))) then acc
                else
                  activate(elem, followup.pos)
                  followup match
                    case Empty(pos) => (acc._1, acc._2 + ((elem.pos, pos)))
                    case next =>
                      (acc._1.enqueue(next -> dir), acc._2 + ((elem.pos, followup.pos)))
          loop(nextElems._1, nextElems._2)
  end loop

  val starting = dir match
    case Direction.Right => elementsByRow(origin.y)(0)
    case Direction.Down  => elementsByColumn(origin.x)(0)
    case Direction.Left  => elementsByRow(origin.y).last
    case Direction.Up    => elementsByColumn(origin.x).last

  activate(starting, origin)
  loop(Queue(starting -> dir), Set())

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
  val horizontal = (0 until lines.length).flatMap: i =>
    List(
      (Coord(0, i), Direction.Right),
      (Coord(lines(0).length() - 1, i), Direction.Left)
    )
  val vertical = (0 until lines(0).length()).flatMap: i =>
    List(
      (Coord(i, 0), Direction.Down),
      (Coord(i, lines.length - 1), Direction.Up)
    )
  val borders = horizontal ++ vertical
  borders.map((coord, dir) => solution(lines, coord, dir)).max

part2
