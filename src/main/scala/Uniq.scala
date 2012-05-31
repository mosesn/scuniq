import scala.io.Source
import java.util.Arrays

object Driver {
  def main(args: Array[String]) {
    val options= parseArgs(args)

    val source = options.input.getOrElse("-") match {
      case "-" => Source.stdin
      case filename => Source.fromFile(filename)
    }

    var counter = 1

    var prevLine: Option[String] = None
    var prevFullLine: String = ""
    for (line <- source.getLines()) {
      val mutantLine = transformLine(line, options)

      prevLine match {
        case Some(contents) if contents equals mutantLine => counter += 1
        case other => {
          printNontrivial(prevLine, prevFullLine, counter, options.setting)
          counter = 1
          prevLine = Some(mutantLine)
          prevFullLine = line
        }
      }
    }

    printNontrivial(prevLine, prevFullLine, counter, options.setting)
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
      makeOptions(args.slice(1, args.length), numChars, numFields, tuple,
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
            makeOptions(args.slice(1, args.length), numChars,
                         args(0).substring(1).toInt, tuple, insensitive)
          }
          else {
            makeOptions(args.slice(2, args.length), numChars,
                         args(1).toInt, tuple, insensitive)
          }
        }
        case 's' => {
          if (args(0).length > 1) {
            makeOptions(args.slice(1, args.length), args(0).substring(1).toInt,
                         numFields, tuple, insensitive)
          }
          else {
            makeOptions(args.slice(2, args.length), args(1).toInt,
                         numFields, tuple, insensitive)
          }
        }
        case other => throw new IllegalArgumentException("Not a valid argument")
      }
    }
  }

  private[this] def parseTuple(
    tuple: Tuple3[Boolean, Boolean, Boolean]): Int = tuple match {
    case Tuple3(true, false, false) => 0
    case Tuple3(false, true, false) => 1
    case Tuple3(false, false, true) => 2
    case Tuple3(false, x, y) => 3
    case other => 4
  }

  private[this] def transformLine(line: String, options: Options): String = {
    val mutated = fieldCut(line, options.numFields).substring(options.numChars)
    if (options.insensitive) {
      mutated.toUpperCase
    }
    else {
      mutated
    }
  }

  private[this] def printNontrivial(prevLine: Option[String],
                                    line: String,
                                    counter: Int,
                                    setting: Int) = prevLine match {
    case None => ""
    case Some(stri) => uniqPrinter(line, counter, setting)
  }

  private[this] def uniqPrinter(line: String, counter: Int, setting: Int) {
    print(setting match {
      case 0 => "%d %s\n".format(counter, line)
      case 1 => if (counter == 0) "%s\n".format(line) else ""
      case 2 => if (counter > 0) "%s\n".format(line) else ""
      case 3 => "%s\n".format(line)
      case other => throw new Exception("Bad argument.")
    })
  }

  private[this] def fieldCut(line: String, numFields: Int): String =
    numFields match {
      case 0 => line
      case x if x < 0 => throw new IllegalArgumentException("Invalid field number")
      case y => """\S+""".r.findAllIn(line).matchData.drop(numFields - 1).next().after.toString
    }

  case class Options(numChars: Int,
                     numFields: Int,
                     setting: Int,
                     insensitive: Boolean,
                     input: Option[String],
                     output: Option[String])
}
