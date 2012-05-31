import scala.io.Source

object Driver {
  def main(args: Array[String]) {
    val options= parseArgs(args)

    val numChars = options.numChars
    val numFields = options.numFields

    val setting = options.setting
    val isInsensitive = options.insensitive

    val source = options.input match {
      case Some("-") => Source.stdin
      case Some(filename) => Source.fromFile(filename)
      case None => Source.stdin
    }
    var counter = 1

    var prevLine: Option[String] = None
    var prevFullLine: String = ""
    for (line <- source.getLines()) {
      val mutantLine = transformLine(line, numFields, numChars, isInsensitive)
      prevLine match {
        case Some(contents) => {
          if (contents == mutantLine) {
            counter += 1
          }
          else {
            printNontrivial(prevLine, prevFullLine, counter, setting)
            counter = 1
            prevLine = Some(mutantLine)
            prevFullLine = line
          }
        }
        case other => {
          printNontrivial(prevLine, prevFullLine, counter, setting)
          counter = 1
          prevLine = Some(mutantLine)
          prevFullLine = line
        }
      }
    }
    printNontrivial(prevLine, prevFullLine, counter, setting)

    source.close()
  }

  private[this] def parseArgs(args: Array[String]): Options = {
    makeOptions(args, 0, 0, Tuple3(false, false, false), false)
  }

  private[this] def makeOptions(args: Array[String],
                                numChars: Int,
                                numFields: Int,
                                tuple: Tuple3[Boolean, Boolean, Boolean],
                                insensitive: Boolean): Options = {
    args.length match {
      case 0 => Options(numChars, numFields, parseTuple(tuple), insensitive, None,
                        None)
      case other => {
        if (args(0).charAt(0) equals '-') {
          processOptions(removeChar(args), numChars, numFields, tuple, insensitive)
        }
        else {
          if (other == 2) {
            Options(numChars, numFields, parseTuple(tuple), insensitive,
                    Some(args(0)), Some(args(1)))
          }
          else if (other == 1) {
            Options(numChars, numFields, parseTuple(tuple), insensitive,
                    Some(args(0)), None)
          }
          else {
            throw new IllegalArgumentException("Wrong number of arguments.")
          }
        }
      }
    }
  }

  private[this] def removeChar(args: Array[String]): Array[String] = {
    val ret = new Array[String](args.length)
    ret(0) = args(0).substring(1)
    for (pos <- 1 until args.length) {
      ret(pos) = args(pos)
    }
    ret
  }

  private[this] def processOptions(args: Array[String],
                                numChars: Int,
                                numFields: Int,
                                tuple: Tuple3[Boolean, Boolean, Boolean],
                                insensitive: Boolean): Options = {
    if (args(0).length == 0) {
      makeOptions(arrayCopy(args, 1, args.length), numChars, numFields, tuple,
                   insensitive)
    }
    else {
      args(0).charAt(0) match {
        case 'c' => processOptions(removeChar(args), numChars, numFields,
                                   Tuple3(true, tuple._2, tuple._3), insensitive)
        case 'd' => processOptions(removeChar(args), numChars, numFields,
                                   Tuple3(tuple._1, true, tuple._3), insensitive)
        case 'u' => processOptions(removeChar(args), numChars, numFields,
                                   Tuple3(tuple._1, tuple._2, true), insensitive)
        case 'i' => processOptions(removeChar(args), numChars, numFields, tuple,
                                   true)
        case 'f' => {
          if (args(0).length > 1) {
            makeOptions(arrayCopy(args, 1, args.length), numChars,
                         args(0).substring(1).toInt, tuple, insensitive)
          }
          else {
            makeOptions(arrayCopy(args, 2, args.length), numChars,
                         args(1).toInt, tuple, insensitive)
          }
        }
        case 's' => {
          if (args(0).length > 1) {
            makeOptions(arrayCopy(args, 1, args.length), args(0).substring(1).toInt,
                         numFields, tuple, insensitive)
          }
          else {
            makeOptions(arrayCopy(args, 2, args.length), args(1).toInt,
                         numFields, tuple, insensitive)
          }
        }
        case other => throw new IllegalArgumentException("Not a valid argument")
      }
    }
  }

  private[this] def arrayCopy[A](arr: Array[A], start: Int, end: Int)(implicit manifest: Manifest[A]): Array[A] = {
    val ret = new Array[A](end - start)
    for (pos <- start until end) {
      ret(pos - start) = arr(pos)
    }
    ret
  }

  private[this] def parseTuple(tuple: Tuple3[Boolean, Boolean, Boolean]): Int = {
    tuple match {
      case Tuple3(true, false, false) => 0
      case Tuple3(false, false, false) => 3
      case Tuple3(false, true, true) => 3
      case Tuple3(false, true, false) => 1
      case Tuple3(false, false, true) => 2
      case other => 4
    }
  }

  private[this] def transformLine(line: String,
                                  numFields: Int,
                                  numChars: Int,
                                  isInsensitive: Boolean): String = {
    val mutated = fieldCut(line, numFields).substring(numChars)
    if (isInsensitive) {
      upperCase(mutated)
    }
    else {
      mutated
    }
  }

  private[this] def printNontrivial(prevLine: Option[String],
                                    line: String,
                                    counter: Int,
                                    setting: Int) {
    prevLine match {
      case None => ""
      case Some(stri) => uniqPrinter(line, counter, setting)
    }
  }

  private[this] def upperCase(line: String): String = {
    line map (_.toUpper)
  }

  private[this] def uniqPrinter(line: String, counter: Int, setting: Int) {
    setting match {
      case 0 => println(counter + " " + line)
      case 1 => if (counter > 0) println(line)
      case 2 => if (counter == 0) println(line)
      case 3 => println(line)
      case other => throw new Exception("Bad argument.")
    }
  }

  private[this] def fieldCut(line: String, numFields: Int): String = {
    numFields match {
      case 0 => line
      case remaining => fieldCut(cutOneField(line), numFields - 1)
    }
  }

  private[this] def cutOneField(line: String): String = {
    line.charAt(0) match {
      case ' ' => cutOneField(line.substring(1))
      case nonSpace => cutNonspaces(line.substring(1))
    }
  }

  private[this] def cutNonspaces(line: String): String = {
    line.charAt(0) match {
      case ' ' => line
      case nonSpace => cutNonspaces(line.substring(1))
    }
  }

  case class Options(numChars: Int,
                     numFields: Int,
                     setting: Int,
                     insensitive: Boolean,
                     input: Option[String],
                     output: Option[String])
}
