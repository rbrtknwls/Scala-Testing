import scala.collection.mutable.ArrayBuffer

class Dot (var state: Int = 0, val xpos: Int, val ypos: Int){
  private var nstate: Int = state

  def changeState(neighbourCount: Int): Unit = {

    neighbourCount match {
      case 2 => ()
      case 3 => nstate = 1
      case _ => nstate = 0
    }

  }

  def printDot(): Unit = {
    state = nstate
    if (state == 1){
      print("■ ")
    } else {
      print("  ")
    }
  }
  def debugPrintDot(): Unit = {
    println(state + "(" + xpos + "," + ypos + ")")
  }
}

class Grid (var widthBound: Int = 2, var heightBound: Int = 2, val initVals : List[List[Int]]){

  private val gridBuffer = new ArrayBuffer[Dot]()

  for (height <- 0 until heightBound) {
    for (width <- 0 until widthBound) {
      if (height < initVals.size && width < initVals(height).size) {
        gridBuffer += new Dot(initVals(height)(width), height, width)
      } else {
        gridBuffer += new Dot(0, height, width)
      }
    }
  }

  def debugGrid(): Unit = {
    for (dot <- gridBuffer) dot.debugPrintDot()
  }

  def printGrid(): Unit = {
    for (height <- 0 until heightBound) {
      println()
      for (width <- 0 until widthBound) {
        gridBuffer(height*widthBound + width).printDot()
      }
    }
    println()
  }

  def updateGrid(): Unit = {

    for (height <- 0 until heightBound) {
      for (width <- 0 until widthBound) {

        var count: Int = 0

        for (ymod <- -1 + height to 1 + height){
          for (xmod <- -1 + width to 1 + width){

            if (ymod >= 0 && xmod >= 0 && ymod < heightBound && xmod < widthBound){
              count += gridBuffer(ymod*widthBound + xmod).state
            }
          }
        }
        count -= gridBuffer(height*widthBound + width).state
        gridBuffer(height*widthBound + width).changeState(count)
      }
    }
  }

  def gameStart(loop: Int): Unit = {

    for (rep <- 1 to loop){
      println("\n ---===" + rep + "===---")
      printGrid()
      updateGrid()
    }
  }
}

object Main extends App {
  val board_start: List[List[Int]] = List(
    List(0, 0, 0, 0, 0),
    List(0, 0, 0, 1, 0),
    List(0, 0, 0, 0, 1),
    List(0, 0, 1, 1, 1),
    List(0, 0, 0, 0, 0))

  val grid = new Grid(10,10, board_start)

  grid.gameStart(20)
}
