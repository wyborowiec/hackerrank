
package wyborowiec


import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._



object Solution {

  // Complete the queensAttack function below.
  def queensAttack(n: Int, k: Int, r_q: Int, c_q: Int, obstacles: Array[Array[Int]]): Int = {
    case class QAPoint(x: Int, y: Int)
    case class QAStraight(a: Int, b: Int, c: Int) {
      def belongs(p: QAPoint): Boolean = a*p.x + b*p.y + c == 0
    }
    val qx = c_q-1
    val qy = r_q-1
    val s1 = QAStraight(1, 0, -qx)
    val s2 = QAStraight(0, 1, -qy)
    val d1 = QAStraight(-1, 1, qx-qy)
    val d2 = QAStraight(1, 1, -qx-qy)
    val ot = obstacles.toSeq.map(a => QAPoint(a(1)-1, a(0)-1))
    // x = 3
    val (lt, gt) = ot.filter(s1.belongs(_)).partition(p => p.x < qx || p.y < qy)
    val ltNum = lt.foldLeft(qy)((acc, point) => Math.min(qy-point.y-1, acc))
    val gtNum = gt.foldLeft(n-qy-1)((acc, point) => Math.min(point.y-qy-1, acc))
    // y = 4
    val (lt1, gt1) = ot.filter(s2.belongs(_)).partition(p => p.x < qx || p.y < qy)
    val ltNum1 = lt1.foldLeft(qx)((acc, point) => Math.min(qx-point.x-1, acc))
    val gtNum1 = gt1.foldLeft(n-qx-1)((acc, point) => Math.min(point.x-qx-1, acc))
    // y = x
    val (lt2, gt2) = ot.filter(d1.belongs(_)).partition(p => p.x < qx || p.y < qy)
    val ltNum2 = lt2.foldLeft(Math.min(qx, qy))((acc, point) => Math.min(Math.abs(qx-point.x)-1, acc))
    val gtNum2 = gt2.foldLeft(Math.min(n-qx-1, n-qy-1))((acc, point) => Math.min(Math.abs(qx-point.x)-1, acc))
    // y = -x
    val (lt3, gt3) = ot.filter(d2.belongs(_)).partition(p => p.x < qx || p.y < qy)
    val ltNum3 = lt3.foldLeft(Math.min(qx, n-qy-1))((acc, point) => Math.min(Math.abs(qx-point.x)-1, acc))
    val gtNum3 = gt3.foldLeft(Math.min(n-qx-1, qy))((acc, point) => Math.min(Math.abs(qx-point.x)-1, acc))
    ltNum + ltNum1 + ltNum2 + ltNum3 + gtNum + gtNum1 + gtNum2 + gtNum3
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nk = stdin.readLine.split(" ")

    val n = nk(0).trim.toInt

    val k = nk(1).trim.toInt

    val r_qC_q = stdin.readLine.split(" ")

    val r_q = r_qC_q(0).trim.toInt

    val c_q = r_qC_q(1).trim.toInt

    val obstacles = Array.ofDim[Int](k, 2)

    for (i <- 0 until k) {
      obstacles(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = queensAttack(n, k, r_q, c_q, obstacles)

    printWriter.println(result)

    printWriter.close()
  }
}


