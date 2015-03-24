package dungeon

import scala.annotation.tailrec
import scala.util._

case class Cellular(born: Seq[Int], survive: Seq[Int], map: Array[Array[Boolean]]) {
  import Cellular._

  @tailrec
  final def iterate(num: Int, f: Option[Cellular => Unit] = None): Cellular = {
    val width = map.length
    val height = map.head.length

    f.foreach { func =>
      func(this)
    }

    val iter = Cellular(born, survive, Array.tabulate(width, height) { (x, y) =>
        def gn = getNeighbor((x, y), _: Int, _: Int, map)
        val count = List(
          gn(-1, -1), gn(0, -1), gn( 1, -1),
          gn(-1,  0),/*  0,  0 */gn( 1,  0),
          gn(-1,  1), gn(0,  1), gn( 1,  1)
        ).count(a => a)

        if (map(x)(y) && survive.contains(count)) {
          true
        } else if (!map(x)(y) && born.contains(count)){
          true
        } else {
          false
        }
      })

    if (num == 0) iter else iter.iterate(num - 1, f)
  }
}

object Cellular {
  def apply(
    width: Int,
    height: Int,
    born: Seq[Int] = Seq(5, 6, 7, 8),
    survive: Seq[Int] = Seq(4, 5, 6, 7, 8),
    aliveChance: Double = 0.50): Cellular = Cellular(born, survive, Array.tabulate(width, height) { (x, y) =>
    Random.nextFloat() < aliveChance
  })

  def getNeighbor(point: (Int, Int), offsetX: Int, offsetY: Int, matrix: Array[Array[Boolean]]) = {
    val x = point._1 + offsetX
    val y = point._2 + offsetY

    Try {
      matrix(x)(y)
    } match {
      case Success(cell) => cell
      case Failure(_) => false
    }
  }
}