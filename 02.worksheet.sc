import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/* -------------------------------------------------------------------------- */
/*                                   Global                                   */
/* -------------------------------------------------------------------------- */
val source = Files
  .readAllLines:
    Path.of:
      "/home/lucasn/Projects/advent-of-code-2023-scala/inputs/day2.txt"
  .asScala

enum Color(val maxValue: Int):
  case red extends Color(12)
  case green extends Color(13)
  case blue extends Color(14)

val gameId = """Game (\d+):.*""".r
val ballFinder = """(\d+) (green|red|blue)""".r

/* -------------------------------------------------------------------------- */
/*                                   Part I                                   */
/* -------------------------------------------------------------------------- */
def part1 =
  source
    .flatMap: game =>
      val possible =
        ballFinder
          .findAllMatchIn(game)
          .forall: e =>
            val List(nb, color) = e.subgroups
            nb.toInt <= Color.valueOf(color).maxValue

      if possible then Some(gameId.unapplySeq(game).get(0).toInt)
      else None
    .sum

/* -------------------------------------------------------------------------- */
/*                                   Part II                                  */
/* -------------------------------------------------------------------------- */
def part2 =
  source.map: game =>
    ballFinder
      .findAllMatchIn(game)
      .map: e =>
        val List(nb, color) = e.subgroups
        (Color.valueOf(color), nb.toInt)
      .toSeq
      .groupMapReduce(_._1)(_._2)((a, b) => if a > b then a else b)
      .foldLeft(1)(_ * _._2)
  .sum
