package dungeon

import scala.reflect.ClassTag
import org.fusesource.jansi.Ansi.Color._
import org.fusesource.jansi.Ansi._
import org.fusesource.jansi.AnsiConsole

package object utils {
  def randomRange(start: Int, end: Int): Int = start + scala.util.Random.nextInt( (end - start) + 1 )
  def randomRange(range: (Int, Int)): Int = randomRange(range._1, range._2)

  def mapOf[A:ClassTag](matrix: Array[Array[A]], f: A => Boolean, filled: String = "■", unfilled: String = "□") =
    matrix.map { row =>
      (row.map { col =>
        if (f(col)) filled else unfilled
      } ++ "\n").mkString("")
    }.mkString("")

  def renderMap[A:ClassTag](matrix: Array[Array[A]], f: A => Boolean, filled: String = "■", unfilled: String = "□"): Unit = {
    val columns = matrix.length
    val rows = matrix.head.length
    //AnsiConsole.systemInstall()
    print(ansi().eraseScreen())
    for {
      x <- 0 until columns
      y <- 0 until rows
    } yield {
      val cell = matrix(x)(y)
      val s = if(f(cell)) filled else unfilled
      //println(s"x: $x, y: $y, s: $s")
      print(ansi().cursor(y, x).a(s))
    }
    //print(ansi().reset())
  }

  def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
  }

  def runDot(): Unit = {
    import sys.process._
    "dot -Tpng dungeon.gv -o dungeon.png" !
  }

  def writeDungeonDot(dot: String): Unit = {
    val data = "digraph G {\n" + dot + "}\n"
    writeToFile("dungeon.gv", data)
    runDot()
  }

  def writeDungeon(dungeon: String): Unit = {
    writeToFile("map.txt", dungeon, append = true)
  }

  def writeToFile(file: String, data: String, append: Boolean = false): Unit = {
    import java.io.File

    val pw = new java.io.PrintWriter(new File(file))
    if (append) {
      try pw.append(data) finally pw.close()
    } else {
      try pw.write(data) finally pw.close()
    }
  }

  def render(c: dungeon.Cellular): Unit = {
    dungeon.utils.renderMap[Boolean](c.map, (a => a), "#", " ")
    Thread.sleep(100)
  }
}
