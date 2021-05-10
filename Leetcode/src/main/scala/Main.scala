import scala.collection.mutable.ArrayBuffer

object Solution_66 {
  def plusOne(digits: Array[Int]): Array[Int] = {
    digits(digits.length-1) = digits(digits.length-1) +1
    digits
  }
}

object Solution_20 {
  var accum: String = "";
  def isValid(s: String): Boolean = {
    s match {
      case "" =>
        val x = accum == ""
        accum = ""
        x
      case s if List("(", "[", "{") contains s.substring(0,1) =>
        accum = s.substring(0,1) + accum
        isValid(s.substring(1))

      case s if accum == "" => false
      case s if s.substring(0,1) == ")" =>
        if (accum.substring(0,1) == "(") {
          accum = accum.substring(1)
          isValid(s.substring(1))
        } else {
          accum = ""
          false
        }

      case s if s.substring(0,1) == "]" =>
        if (accum.substring(0,1) == "["){
          accum = accum.substring(1)
          isValid(s.substring(1))
        } else {
          accum = ""
          false

        }
      case s if s.substring(0,1) == "}" => {
        if (accum.substring(0,1) == "{"){
          accum = accum.substring(1)
          isValid(s.substring(1))
        } else {
          accum = ""
          false

        }
      }
    }
  }
}

object Main extends App {

}
