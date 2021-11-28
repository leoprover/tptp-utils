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
  private[this] var command: Option[Command] = None
  private[this] var tstpOutput: Boolean = false

  sealed abstract class Command
  final case object Parse extends Command
  final case object Reparse extends Command
  final case class  Transform(goal: TPTP.AnnotatedFormula.FormulaType.FormulaType) extends Command

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
      var error: Option[String] = None

      try {
        parseArgs(args.toSeq)
        // Allocate output file
        outfile = Some(if (outputFileName.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outputFileName.get)))
        // Read input
        infile = Some(if (inputFileName == "-") io.Source.stdin else io.Source.fromFile(inputFileName))
        // Parse input
        val parsedInput = TPTPParser.problem(infile.get)
        var result: String = ""
        command match {
          case Some(command0) => command0 match {
            case Parse =>
              // If we ended up here, parsing was succesful.
              result = s"% SZS status Success for $inputFileName"
            case Reparse =>
              // If we ended up here, parsing was succesful.
              result = s"% SZS status Success for $inputFileName"
              // TODO
            case Transform(goal) => ???
          }
          case None => ???
        }
        outfile.get.print(result)
        outfile.get.flush()
      } catch {
        case e: IllegalArgumentException =>
          error = Some(e.getMessage)
          if (!tstpOutput) usage()
        case e: FileNotFoundException =>
          error = Some(s"File cannot be found or is not readable/writable: ${e.getMessage}")
        case e: TPTPParser.TPTPParseException =>
          error = Some(s"Input file could not be parsed, parse error at ${e.line}:${e.offset}: ${e.getMessage}")
        case e: Throwable =>
          error = Some(s"Unexpected error: ${e.getMessage}. This is considered an implementation error; please report this!")
      } finally {
        if (error.nonEmpty && tstpOutput) {
          if (outfile.isDefined) {
            outfile.get.println(s"% SZS status Error for $inputFileName : ${error.get}\n")
            outfile.get.flush()
          } else println(s"% SZS status Error for $inputFileName : ${error.get}\n")
        }
        infile.foreach(_.close())
        outfile.foreach(_.close())
      }
      if (error.nonEmpty) {
        if (!tstpOutput) println(error.get)
        System.exit(1)
      }
    }
  }

  private[this] final def printVersion(): Unit = {
    println(s"$name $version")
  }

  private[this] final def usage(): Unit = {
    println(s"usage: $name [--tstp] <command> [command parameters] <problem file> [<output file>]")
    println(
      """
        | <command> is the command to be executed (parse, transform).
        | <problem file> can be either a file name or '-' (without quotes) for stdin.
        | If <output file> is specified, the result is written to <output file>, otherwise to stdout.
        |
        | Commands:
        |  parse
        |     Parse the problem and return SZS Success if successful; SZS SyntaxError otherwise.
        |  reparse
        |     Parse the problem and, if successful, print the AST of the parsed problem in a
        |     JSON-based format.
        |  transform
        |     Parse a problem, and transform and print it in a different TPTP language. This is possible
        |     if the goal language is at least as expressive as the source language, e.g.
        |     transform a FOF problem into a THF problem. Auxiliary formulae might be added if necessary,
        |     e.g., new type declarations.
        |     The goal language is specified as command parameter using one of the following values:
        |     cnf, tcf, fof, tff, thf
        |
        | Options:
        |  --tstp
        |     Enable TSTP-compatible output: The output in <output file> (or stdout) will
        |     start with a SZS status value and the output will be wrapped within
        |     SZS BEGIN and SZS END block delimiters. Disabled by default.
        |  --version
        |     Prints the version number of the executable and terminates.
        |  --help
        |     Prints this description and terminates.
        |""".stripMargin)
  }

  private[this] final def parseArgs(args: Seq[String]): Unit = {
    ???
  }
}
