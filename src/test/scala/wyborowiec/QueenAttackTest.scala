package wyborowiec


import org.scalatest._
import flatspec._
import matchers._

class QueenAttackTest extends AnyFlatSpec with should.Matchers {

  "A sample 0" should "work" in {
    Solution.queensAttack(4, 0, 4, 4, new Array[Array[Int]](0)) should be (9)
  }

  "A sample 1" should "work" in {
    val a = Array(
      Array(5,5),
      Array(4,2),
      Array(2,3),
      Array(3,4)
    )
    Solution.queensAttack(5, 4, 4, 3, a) should be (8)
  }

  "A sample 12" should "work" in {
    val a = Array(
      Array(5,5),
      Array(4,2),
      Array(2,3)
    )
    Solution.queensAttack(5, 3, 4, 3, a) should be (10)
  }

  "A sample 2" should "work" in {
    val a = Array[Array[Int]]()
    Solution.queensAttack(1, 0, 1, 1, a) should be (0)
  }

  "A sample 4" should "work" in {
    val a = Array(
      Array(5,5),
      Array(4,2),
      Array(2,3),
      Array(3,2)
    )
    Solution.queensAttack(5, 4, 4, 3, a) should be (8)
  }

  "A sample 5" should "work" in {
    val a = Array(
      Array(5,5),
      Array(4,2),
      Array(2,3),
      Array(3,2),
      Array(2,1),
      Array(5,4),
    )
    Solution.queensAttack(5, 5, 4, 3, a) should be (7)
  }

  "A sample 6" should "work" in {
    val a = Array(
      Array(5,5),
      Array(4,2),
      Array(2,3),
      Array(3,2),
      Array(2,1),
      Array(5,3),
      Array(5,4)
    )
    Solution.queensAttack(5, 6, 4, 3, a) should be (6)
  }

  "A sample 7" should "work" in {
    val a = Array[Array[Int]]()
    Solution.queensAttack(1, 1, 1, 1, a) should be (0)
  }

  "A sample 8" should "work" in {
    val a: Array[Array[Int]] = (for (
      i <- 1 to 10;
      j <- 1 to 10
    ) yield Array(i,j))
      .filter(ia => ia(0)!=1 || ia(1)!=1)
      .toArray
    Solution.queensAttack(1000, 999, 1, 1, a) should be (0)
  }

  "A sample 9" should "work" in {
    val a = Array[Array[Int]]()
    Solution.queensAttack(1000, 999, 1, 1, a) should be (3*999)
  }

  "A sample 10" should "work" in {
    val a: Array[Array[Int]] = (for (
      i <- 1 to 10;
      j <- 1 to 10
    ) yield Array(i,j))
      .filter(ia => ia(0)!=1 && ia(1)!=1)
      .toArray
    Solution.queensAttack(1000, 999, 1, 1, a) should be (2*999)
  }

  "A sample 11" should "work" in {
    val a = Array(
      Array(54,87),
      Array(64,97),
      Array(42,75),
      Array(32,65),
      Array(42,87),
      Array(32,97),
      Array(54,75),
      Array(64,65),
      Array(48,87),
      Array(48,75),
      Array(54,81),
      Array(42,81),
      Array(45,17),
      Array(14,24),
      Array(35,15),
      Array(95,64),
      Array(63,87),
      Array(25,72),
      Array(71,38),
      Array(96,97),
      Array(16,30),
      Array(60,34),
      Array(31,67),
      Array(26,82),
      Array(20,93),
      Array(81,38),
      Array(51,94),
      Array(75,41),
      Array(79,84),
      Array(79,65),
      Array(76,80),
      Array(52,87),
      Array(81,54),
      Array(89,52),
      Array(20,31),
      Array(10,41),
      Array(32,73),
      Array(83,98),
      Array(87,61),
      Array(82,52),
      Array(80,64),
      Array(82,46),
      Array(49,21),
      Array(73,86),
      Array(37,70),
      Array(43,12),
      Array(94,28),
      Array(10,93),
      Array(52,25),
      Array(50,61),
      Array(52,68),
      Array(52,23),
      Array(60,91),
      Array(79,17),
      Array(93,82),
      Array(12,18),
      Array(75,64),
      Array(69,69),
      Array(94,74),
      Array(61,61),
      Array(46,57),
      Array(67,45),
      Array(96,64),
      Array(83,89),
      Array(58,87),
      Array(76,53),
      Array(79,21),
      Array(94,70),
      Array(16,10),
      Array(50,82),
      Array(92,20),
      Array(40,51),
      Array(49,28),
      Array(51,82),
      Array(35,16),
      Array(15,86),
      Array(78,89),
      Array(41,98),
      Array(70,46),
      Array(79,79),
      Array(24,40),
      Array(91,13),
      Array(59,73),
      Array(35,32),
      Array(40,31),
      Array(14,31),
      Array(71,35),
      Array(96,18),
      Array(27,39),
      Array(28,38),
      Array(41,36),
      Array(31,63),
      Array(52,48),
      Array(81,25),
      Array(49,90),
      Array(32,65),
      Array(25,45),
      Array(63,94),
      Array(89,50),
      Array(43,41)

    )
    Solution.queensAttack(100, 100, 48, 81, a) should be (40)
  }


}