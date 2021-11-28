package leo.modules

import leo.datastructures.TPTP
import leo.datastructures.TPTP.{AnnotatedFormula, Problem}
import leo.modules.tptputils.{SyntaxTransform, TPTPTransformException}
import leo.modules.input.TPTPParser

import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}

object TPTPUtilsApp {
  final val name: String = "tptputils"
  final val version: Double = 1.0

  private[this] var inputFileName = ""
  private[this] var outputFileName: Option[String] = None
  private[this] var command: Option[String] = None

  final def main(args: Array[String]): Unit = {
    if (args.contains("--help")) {
      usage(); return
    }
    if (args.contains("--version")) {
      printVersion(); return
    }
    if (args.isEmpty) usage()
    else {
      var infile: Option[Source] = None
      var outfile: Option[PrintWriter] = None
      var error: Boolean = false
      try {
        parseArgs(args.toSeq)
        // Read input
        infile = Some(if (inputFileName == "-") io.Source.stdin else io.Source.fromFile(inputFileName))
        // Parse input
        val parsedInput = TPTPParser.problem(infile.get)
      } catch {
        case e: IllegalArgumentException =>
          println(e.getMessage)
          usage()
          error = true
        case e: FileNotFoundException =>
          println(s"File cannot be found or is not readable/writable: ${e.getMessage}")
          error = true
        case e: TPTPParser.TPTPParseException =>
          println(s"Input file could not be parsed, parse error at ${e.line}:${e.offset}: ${e.getMessage}")
          error = true
        case e: Throwable =>
          println(s"Unexpected error. ${e.getMessage}")
          println("This is considered an implementation error; please report this!")
          error = true
      } finally {
        infile.foreach(_.close())
        outfile.foreach(_.close())
      }
      if (error) System.exit(1)
    }
  }

  private[this] final def printVersion(): Unit = {
    println(s"$name $version")
  }

  private[this] final def usage(): Unit = {
    println(s"usage: $name <command> <problem file> [<output file>]")
    println(
      """
        | <command> is the command to be executed (parse, transform).
        | <problem file> can be either a file name or '-' (without parentheses) for stdin.
        | If <output file> is specified, the result is written to <output file>, otherwise to stdout.
        |
        | Commands:
        |  parse     - ...
        |  transform - ...
        |
        | Options:
        |  --version
        |     Prints the version number of the executable and terminates.
        |
        |  --help
        |     Prints this description and terminates.
        |""".stripMargin)
  }

  private[this] final def parseArgs(args: Seq[String]): Unit = {
    ???
  }
}
