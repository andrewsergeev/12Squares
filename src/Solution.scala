import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Solution {

  val figureDigit = 10

  case class Square(topLeft: Int, topRight: Int, bottomLeft: Int, bottomRight: Int)

  def main(args: Array[String]): Unit = {
    var figure = mutable.ListBuffer.empty[Square]

    for (_ <- 1 to 12) {
      val tmp = StdIn.readLine().split(" ").map(_.toInt)
      val square = Square(tmp(0), tmp(1), tmp(2), tmp(3))

      figure += square
    }

    permutations(figure, figure.length)
  }

  def permutations(figure: ListBuffer[Square], n: Int): Unit = {
    if (!isCorrectFigure(figure, n)) return
    if (n == 1) {
      figure.foreach(square => println(square.topLeft + " " + square.topRight + " " + square.bottomLeft + " " + square.bottomRight))
      println()
      return
    }
    for (i <- 0 until n) {
      swap(figure, i, n - 1)
      permutations(figure, n - 1)
      swap(figure, i, n - 1)
    }
  }

  def swap(figure: ListBuffer[Square], a: Int, b: Int): ListBuffer[Square] = {
    val tmp = figure(a)
    figure(a) = figure(b)
    figure(b) = tmp

    figure
  }

  def isCorrectFigure(figure: ListBuffer[Square], n: Int): Boolean = {
    if ((n == 1 && figure(0).topRight + figure(1).topLeft > figureDigit) ||
      (n == 1 && figure(0).bottomLeft + figure(3).topLeft + figure(2).topRight > figureDigit) ||
      (n == 1 && fourSquareSum(figure(0), figure(1), figure(4), figure(3)) != figureDigit) ||
      (n == 1 && figure(1).bottomRight + figure(4).topRight + figure(5).topLeft > figureDigit) ||
      (n == 2 && figure(2).bottomLeft + figure(6).topLeft > figureDigit) ||
      (n == 2 && fourSquareSum(figure(2), figure(3), figure(7), figure(6)) != figureDigit) ||
      (n == 3 && fourSquareSum(figure(3), figure(4), figure(8), figure(7)) != figureDigit) ||
      (n == 4 && fourSquareSum(figure(4), figure(5), figure(9), figure(8)) != figureDigit) ||
      (n == 5 && figure(5).bottomRight + figure(9).topRight > figureDigit) ||
      (n == 6 && figure(6).bottomRight + figure(7).bottomLeft + figure(10).topLeft > figureDigit) ||
      (n == 7 && fourSquareSum(figure(7), figure(8), figure(11), figure(10)) != figureDigit) ||
      (n == 8 && figure(9).bottomLeft + figure(8).bottomRight + figure(11).topRight > figureDigit) ||
      (n == 10 && figure(10).bottomRight + figure(11).bottomLeft > figureDigit)) {
      false
    } else {
      true
    }
  }

  def fourSquareSum(tl: Square, tr: Square, br: Square, bl: Square): Int = tl.bottomRight + tr.bottomLeft + br.topLeft + bl.topRight

}
