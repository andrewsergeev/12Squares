import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Comparison {

  val figureDigit = 10

  case class Square(topLeft: Int, topRight: Int, bottomLeft: Int, bottomRight: Int)

  def main(args: Array[String]): Unit = {

    var figure = mutable.ListBuffer.empty[Square]

    for (_ <- 1 to 12) {
      val tmp = StdIn.readLine().split(" ").map(_.toInt)
      val square = Square(tmp(0), tmp(1), tmp(2), tmp(3))

      figure += square
    }

    var first: Long = 0
    var second: Long = 0

    println("----------------- Scala permutations -----------------")
    first = java.lang.System.currentTimeMillis
    figure.permutations.foreach(figure => {
      if (isCorrectFigureOld(figure)) {
        figure.foreach(square => println(square.topLeft + " " + square.topRight + " " + square.bottomLeft + " " + square.bottomRight))
        println()
      }
    })
    second = java.lang.System.currentTimeMillis
    println("Время выполнения: " + (second-first))

    println("----------------- Recursive permutations -----------------")
    first = java.lang.System.currentTimeMillis
    permutations(figure, figure.length)
    second = java.lang.System.currentTimeMillis
    println("Время выполнения: " + (second-first))

    println("----------------- Recursive permutations with clipping -----------------")
    first = java.lang.System.currentTimeMillis
    permutationsWithClipping(figure, figure.length)
    second = java.lang.System.currentTimeMillis
    println("Время выполнения: " + (second-first))

  }

  def permutationsWithClipping(figure: ListBuffer[Square], n: Int): Unit = {
    if (!isCorrectFigure(figure, n)) return
    if (n == 1) {
      figure.foreach(square => println(square.topLeft + " " + square.topRight + " " + square.bottomLeft + " " + square.bottomRight))
      println()
      return
    }
    for (i <- 0 until n) {
      swap(figure, i, n - 1)
      permutationsWithClipping(figure, n - 1)
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
    if (n == 1 && figure(0).topRight + figure(1).topLeft > figureDigit) return false
    if (n == 1 && figure(0).bottomLeft + figure(3).topLeft + figure(2).topRight > figureDigit) return false
    if (n == 1 && fourSquareSum(figure(0), figure(1), figure(4), figure(3)) != figureDigit) return false
    if (n == 1 && figure(1).bottomRight + figure(4).topRight + figure(5).topLeft > figureDigit) return false
    if (n == 2 && figure(2).bottomLeft + figure(6).topLeft > figureDigit) return false
    if (n == 2 && fourSquareSum(figure(2), figure(3), figure(7), figure(6)) != figureDigit) return false
    if (n == 3 && fourSquareSum(figure(3), figure(4), figure(8), figure(7)) != figureDigit) return false
    if (n == 4 && fourSquareSum(figure(4), figure(5), figure(9), figure(8)) != figureDigit) return false
    if (n == 5 && figure(5).bottomRight + figure(9).topRight > figureDigit) return false
    if (n == 6 && figure(6).bottomRight + figure(7).bottomLeft + figure(10).topLeft > figureDigit) return false
    if (n == 7 && fourSquareSum(figure(7), figure(8), figure(11), figure(10)) != figureDigit) return false
    if (n == 8 && figure(9).bottomLeft + figure(8).bottomRight + figure(11).topRight > figureDigit) return false
    if (n == 10 && figure(10).bottomRight + figure(11).bottomLeft > figureDigit) return false

    true
  }

  def permutations(figure: ListBuffer[Square], n: Int): Unit = {
    if (n == 1) {
      if (isCorrectFigureOld(figure)) {
        figure.foreach(square => println(square.topLeft + " " + square.topRight + " " + square.bottomLeft + " " + square.bottomRight))
        println()
      }
      return
    }

    for (i <- 0 until n) {
      swap(figure, i, n - 1)
      permutations(figure, n - 1)
      swap(figure, i, n - 1)
    }

  }

  def isCorrectFigureOld(figure: ListBuffer[Square]): Boolean = {
      if (figure(0).topRight + figure(1).topLeft > figureDigit) return false                                             // n == 2
      if (figure(0).bottomLeft + figure(3).topLeft + figure(2).topRight > figureDigit) return false                      // n == 4
      if (fourSquareSum(figure(0), figure(1), figure(4), figure(3)) != figureDigit) return false                         // n == 5
      if (figure(1).bottomRight + figure(4).topRight + figure(5).topLeft > figureDigit) return false                     // n == 6
      if (figure(2).bottomLeft + figure(6).topLeft > figureDigit) return false                                           // n == 7
      if (fourSquareSum(figure(2), figure(3), figure(7), figure(6)) != figureDigit) return false                         // n == 8
      if (fourSquareSum(figure(3), figure(4), figure(8), figure(7)) != figureDigit) return false                         // n == 9
      if (fourSquareSum(figure(4), figure(5), figure(9), figure(8)) != figureDigit) return false                         // n == 10
      if (figure(5).bottomRight + figure(9).topRight > figureDigit) return false                                         // n == 10
      if (figure(6).bottomRight + figure(7).bottomLeft + figure(10).topLeft > figureDigit) return false                  // n == 11
      if (fourSquareSum(figure(7), figure(8), figure(11), figure(10)) != figureDigit) return false                       // n == 12
      if (figure(9).bottomLeft + figure(8).bottomRight + figure(11).topRight > figureDigit) return false                 // n == 12
      if (figure(10).bottomRight + figure(11).bottomLeft > figureDigit) return false                                     // n == 12

    true
  }

  def fourSquareSum(tl: Square, tr: Square, br: Square, bl: Square): Int = tl.bottomRight + tr.bottomLeft + br.topLeft + bl.topRight

}

/*
#1
1 0 7 5
3 4 1 3
5 1 1 4
7 6 1 8
5 7 4 4
8 3 3 4
7 1 5 4
4 1 1 0
3 1 1 4
2 4 3 8
5 1 1 7
4 2 4 2

#2
8 0 5 1
4 4 3 6
3 7 1 2
5 0 5 2
3 0 4 3
5 1 0 7
4 0 3 3
5 4 6 0
3 4 3 0
1 7 3 8
0 7 7 3
3 2 0 4

#3
1 6 4 0
6 1 3 5
4 0 5 2
4 1 3 5
5 1 4 2
6 5 5 4
4 0 4 6
5 2 1 4
4 6 0 1
2 0 1 4
3 0 4 5
4 0 5 1

#4
3 3 3 4
4 1 3 5
4 5 3 4
5 2 1 3
3 3 7 4
2 1 3 3
3 2 6 1
4 3 3 4
5 2 2 2
4 2 2 3
5 5 1 4
1 2 5 3

Correct Figure
4 0 0 3
3 3 3 5
6 3 6 6
2 2 2 7
2 1 1 3
2 2 4 7
3 1 1 3
1 1 3 4
1 2 4 1
1 1 1 9
3 1 8 2
1 1 3 7
*/
