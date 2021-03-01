package wyborowiec

import org.scalatest._
import flatspec._
import matchers._

class ConvexPolygonsTest extends AnyFlatSpec with should.Matchers {

  "A rectangle" should "be convex" in {
    val l = List(Point(0,0), Point(0,1), Point(1,1), Point(1,0))
    ConvexPolygons.convex(l) should be (true)
  }

  "A shape" should "be concave" in {
    val l = List(Point(0,0), Point(0,4), Point(4,4), Point(7,7), Point(7,0))
    ConvexPolygons.convex(l) should be (false)
  }

  "Another shape" should "be convex" in {
    val l = List(Point(0,0), Point(0,4), Point(4,4), Point(7,3), Point(7,0))
    ConvexPolygons.convex(l) should be (true)
  }

  "A triangle" should "be convex" in {
    val l = List(Point(0,0), Point(0,1), Point(1, 0))
    ConvexPolygons.convex(l) should be (true)
  }

  "A shape with 3 vertexes on a straight" should "be concave" in {
    val l = List(Point(0,0), Point(0,4), Point(4,4), Point(7,4), Point(7,0))
    ConvexPolygons.convex(l) should be (false)
  }

  "A degenerated triangle" should "be concave" in {
    val l = List(Point(0,0), Point(0,1), Point(0,2))
    ConvexPolygons.convex(l) should be (false)
  }

  "Number of points below 3" should "throw IllegalArgumentException" in {
    val l = List(Point(0,0), Point(0,1))
    a [IllegalArgumentException] should be thrownBy {
      ConvexPolygons.convex(l)
    }
  }

}
