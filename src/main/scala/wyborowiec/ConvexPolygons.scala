package wyborowiec

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.Breaks.break

case class Point(x: Double, y: Double)
case class Straight(a: Double, b: Double, c: Double) {
  def pointsOnTheSameSide(p1: Point, p2: Point): Boolean = (a*p1.x + b*p1.y + c)*(a*p2.x + b*p2.y + c) > 0
  def belongs(p: Point): Boolean = a*p.x + b*p.y + c == 0
}

object Straight {
  def apply(p1: Point, p2: Point): Straight = {
    val a = p1.y-p2.y
    val b = p2.x-p1.x
    val c = p1.x*p2.y - p1.y*p2.x
    Straight(a, b, c)
  }
}

object ConvexPolygons {
  def main(args: Array[String]): Unit = {
    println("Convex polygons")
    val l = readInput()
    println(l)
    println(neighbours(l))

  }

  def convex(l: List[Point]): Boolean = {
    if (l.size < 3){
      throw new IllegalArgumentException("Min number of vertexes is 3")
    } else if (l.size == 3) {
      !Straight(l(0), l(1)).belongs(l(2))
    } else
      check(0, l)
  }

  @tailrec
  def check(i: Int, l: List[Point]): Boolean = {
    if (i == l.size) true
    else {
      val v1 = l(i)
      val v2 = l((i + 1) % l.size)
      val s = Straight(v1, v2)
      val v3 = l((i + 2) % l.size)
      @tailrec
      def run(j: Int): Boolean = {
        if (j == l.size - 3) true
        else {
          val vn = l((i + 3 + j) % l.size)
          s.pointsOnTheSameSide(v3, vn) && run(j + 1)
        }
      }
      if (!run(0)) false
      else check(i + 1, l)
    }
  }

  def neighbours(l: List[Point]): List[(Point, Point)] = {
    l.foldLeft((l.reverse.head, List[(Point, Point)]()))(
      (acc, point) => acc match {
        case (prev, result) => (point, result :+ (prev, point))
      }
    )._2
  }

  def readInput() = {
    Source.fromFile("src/main/resources/points.in").getLines().toList
      .map(_.split(' '))
      .map(a => Point(a(0).toDouble, a(1).toDouble))
  }


}