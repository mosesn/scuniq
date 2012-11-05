import scala.io.Source
import java.util.Arrays
import com.mosesn.pirate.Pirate

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
  case class Options(numChars: Int,
                     numFields: Int,
                     setting: Int,
                     insensitive: Boolean,
                     input: Option[String],
                     output: Option[String])

  private[this] def parseArgs(args: Array[String]): Options = {
    val a = Pirate("[-cdu] [-i] [-f int] [-s int] [input [output]]")(args)
    Options(a.intMap.getOrElse('f', 0), a.intMap.getOrElse('s', 0), parseTuple(a.flags.contains('c'), a.flags.contains('d'), a.flags.contains('u')), a.flags.contains('i'), a.strings.get("input"), a.strings.get("output"))
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

}
