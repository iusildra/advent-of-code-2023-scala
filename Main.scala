def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

case class Coord(x: Int, y: Int)
def adjacentCoords(start: Coord, end: Coord) =
  (start.x - 1 to end.x + 1).flatMap: x =>
    List(Coord(x, start.y - 1), Coord(x, start.y + 1))
  ++ IndexedSeq(Coord(start.x - 1, start.y), Coord(end.x + 1, end.y))
