package leo.modules

import leo.datastructures.TPTP
import leo.datastructures.TPTP.Problem
import leo.modules.tptputils.{Linter, ParseTree, SyntaxDowngrade, SyntaxTransform, Normalization}
import leo.modules.input.TPTPParser

import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}

object TPTPUtilsApp {
  final val name: String = "tptputils"
  final val version: String = "1.3.4"

  private[this] var inputFileName = ""
  private[this] var outputFileName: Option[String] = None
  private[this] var command: Option[Command] = None
  private[this] var tstpOutput: Boolean = false

  final class FileAlreadyExistsException(msg: String) extends RuntimeException(msg)

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
        val result = command.get match {
          case Parse =>
            val result = TPTPParser.problem(infile.get)
            generateResultWithPrefix(result.pretty, "Success", "")
          case Reparse =>
            val parsedInput = TPTPParser.problem(infile.get)
            val json: String = ParseTree(parsedInput)
            generateResultWithPrefix(json, "Success", "LogicalData")
          case Transform(goal) =>
            val parsedInput = TPTPParser.problem(infile.get)
            val transformed = SyntaxTransform(goal, parsedInput)
            generateResultWithPrefix(tptpProblemToString(transformed), "Success", "ListOfFormulae")
          case Downgrade(goal) =>
            try {
              val parsedInput = TPTPParser.problem(infile.get)
              val transformed = SyntaxDowngrade(goal, parsedInput)
              generateResultWithPrefix(tptpProblemToString(transformed), "Success", "ListOfFormulae")
            } catch {
              case e: IllegalArgumentException => generateResultWithPrefix("", "InputError", "", e.getMessage)
            }
          case Lint =>
            val parsedInput = TPTPParser.problem(infile.get)
            val lint = Linter(parsedInput)
            val result = lint.mkString("\n")
            generateResultWithPrefix(result, "Success", "LogicalData")
          case Import(from) =>
            val result = tptputils.Import(infile.get, from)
            generateResultWithPrefix(tptpProblemToString(result), "Success", "ListOfFormulae")
          case Export(_) => ???
          case Normalize(normalform) =>
            val parsedInput = TPTPParser.problem(infile.get)
            val result = tptputils.Normalization(normalform,parsedInput)
            generateResultWithPrefix(tptpProblemToString(result), "Success", "ListOfFormulae")
          case Fragment =>
            val parsedInput = TPTPParser.problem(infile.get)
            val result = tptputils.Fragments.apply(parsedInput)
            generateResultWithPrefix(tptpProblemToString(result), "Success", "ListOfFormulae")
        }
        outfile.get.print(result)
        outfile.get.flush()
      } catch {
        case e: IllegalArgumentException =>
          error = Some(e.getMessage)
          if (!tstpOutput) usage()
        case e: FileNotFoundException =>
          error = Some(s"File cannot be found or is not readable/writable: ${e.getMessage}")
        case e: FileAlreadyExistsException =>
          error = Some(e.getMessage)
        case e: TPTPParser.TPTPParseException =>
          error = Some(s"Input file could not be parsed, parse error at ${e.line}:${e.offset}: ${e.getMessage}")
        case e: leo.modules.tptputils.UnsupportedInputException =>
          error = Some(s"Input is inappropriate for the operation: ${e.getMessage}")
        case e: Throwable =>
          error = Some(s"Unexpected error: ${e.getMessage} (${e.printStackTrace()}). This is considered an implementation error; please report this!")
      } finally {
        if (error.nonEmpty) {
          if (tstpOutput) {
            if (outfile.isDefined) {
              if (inputFileName.nonEmpty) outfile.get.println(s"% SZS status Error for $inputFileName : ${error.get}\n")
              else outfile.get.println(s"% SZS status Error : ${error.get}\n")
              outfile.get.flush()
            } else {
              if (inputFileName.nonEmpty) println(s"% SZS status Error for $inputFileName : ${error.get}\n")
              else println(s"% SZS status Error : ${error.get}\n")
            }
          } else {
            if (outfile.isDefined) {
              outfile.get.println(s"Error: ${error.get}")
              outfile.get.flush()
              if (outputFileName.isDefined) {
                if (outputFileName.get != "-") System.err.println(s"Error: ${error.get}")
              }
            } else println(s"Error: ${error.get}")
          }
        }
        try {
          infile.foreach(_.close())
          outfile.foreach(_.close())
        } catch {
          case _: Throwable => ()
        }
        if (error.nonEmpty) System.exit(1)
      }
    }
  }

  def tptpProblemToString(problem: Problem): String = {
    val sb: StringBuilder = new StringBuilder()
    problem.includes foreach { case (file, (selection, _)) =>
      if (selection.isEmpty) sb.append(s"include('$file').\n")
      else sb.append(s"include('$file', ${selection.mkString("[",",","]")}).\n")
    }
    problem.formulas foreach { f =>
      sb.append(s"${f.pretty}\n")
    }
    if (problem.includes.nonEmpty || problem.formulas.nonEmpty) sb.dropRight(1).toString()
    else sb.toString()
  }

  private[this] final def generateResultWithPrefix(result: String, szsStatus: String, szsDatatype: String, extraTSTPMessage: String = ""): String = {
    import java.util.Calendar

    val sb: StringBuilder = new StringBuilder()
    if (result.nonEmpty) {
      sb.append(s"%%% This output was generated by $name, version $version.\n")
      sb.append(s"%%% Generated on ${Calendar.getInstance().getTime.toString} using command '${command.get.toString.toLowerCase}'.\n")
      if (tstpOutput) {
        if (extraTSTPMessage == null || extraTSTPMessage.isEmpty) sb.append(s"% SZS status $szsStatus for $inputFileName\n")
        else sb.append(s"% SZS status $szsStatus for $inputFileName : $extraTSTPMessage\n")
      }
      sb.append(generateResult(result, szsStatus, szsDatatype, extraTSTPMessage))
    }
    sb.toString()
  }
  private[this] final def generateResult(result: String, szsStatus: String, szsDatatype: String, extraTSTPMessage: String = ""): String = {
    val sb: StringBuilder = new StringBuilder()
    if (result.nonEmpty) {
      if (tstpOutput) sb.append(s"% SZS output start $szsDatatype for $inputFileName\n")
      if (result.nonEmpty) sb.append(result)
      sb.append("\n")
      if (tstpOutput) sb.append(s"% SZS output end $szsDatatype for $inputFileName\n")
    }
    sb.toString()
  }

  private[this] final def printVersion(): Unit = {
    println(s"$name $version")
  }

  private[this] final def usage(): Unit = {
    println(s"usage: $name [options] <command> [command parameters] <problem file>")
    println(
      """
        | <command> is the command to be executed (see below). <problem file> can be
        | either a file name or '-' (without quotes) for stdin. If <output file> is
        | specified, the result is written to <output file>, otherwise to stdout.
        |
        | Commands:
        |  parse        Parse the problem and return SZS Success if successful;
        |               SZS SyntaxError otherwise.
        |  reparse      Parse the problem and, if successful, print the AST of
        |               the parsed problem in a JSON-based format.
        |  transform    Parse a problem, and transform and print it in a different
        |               TPTP language. This is possible if the goal language is at
        |               least as expressive as the source language, e.g. transforming
        |               a FOF problem into a THF problem. Auxiliary formulae might be
        |               added if necessary, e.g., new type declarations.
        |
        |               The goal language is specified as a mandatory command parameter
        |               using one of the following values:
        |               --CNF, --TCF, --FOF, --TFF, --THF
        |  downgrade    Parse a problem, transform and print it in a less expressive TPTP
        |               language. This will fail if the problem contains formulae that are
        |               not directly expressible in the goal language, e.g., lambdas in THF
        |               when transforming to TFF and similar.
        |               If the goal language is more expressive
        |               instead, then `transform` will be executed instead.
        |
        |               The goal language is specified as a mandatory command parameter
        |               using one of the following values:
        |               --TFF
        |
        |  lint         Inspect the input problem for suspicious constructs, unused symbols,
        |               malformed logic specifications, etc.
        |
        |  import       Translate, if possible, the input file into an adequate TPTP
        |               representation.
        |
        |               The source language is specified as a mandatory command parameter:
        |               --LRML   (for import from LegalRuleML)
        |
        |  normalize    Transform the input wrt. some normal form given as parameter.
        |               Valid parameters are (more to come):
        |               --prenex (for prenex normal form)
        |
        | fragment      Analyze the input whether it is member of some known fragment of FOL.
        |               Works only for FOF/TFF inputs, and only for inputs with a single
        |               annotated formula.
        |               Can recognize: Bernay-Schönfinkel-Ramsey (that's it, more to come.)
        |
        | Options:
        |  --tstp       Enable TSTP-compatible output: The output in <output file>
        |               (or stdout) will start with a SZS status value and the output
        |               will be wrapped within SZS BEGIN and SZS END block delimiters.
        |               Disabled by default.
        |
        |  --output <output file>
        |               Write output to <output file> instead of stdout.
        |
        |  --version    Print the version number of the executable and terminate.
        |
        |  --help       Print this description and terminate.
        |""".stripMargin)
  }

  protected sealed abstract class Command
  final private case object Parse extends Command
  final private case object Reparse extends Command
  final private case class  Transform(goal: TPTP.AnnotatedFormula.FormulaType.FormulaType) extends Command
  final private case class  Downgrade(goal: TPTP.AnnotatedFormula.FormulaType.FormulaType) extends Command
  final private case object Lint extends Command
  final private case class  Import(from: leo.modules.tptputils.Import.ExternalLanguage) extends Command
  final private case class  Export(to: leo.modules.tptputils.Import.ExternalLanguage) extends Command
  final private case class Normalize(normalform: Normalization.Normalform) extends Command
  final private case object Fragment extends Command

  private[this] final def parseArgs(args: Seq[String]): Unit = {
    var args0 = args
    try {
      var hd = args0.head
      while (hd.startsWith("--")) { // Optional flags
        hd match {
          case "--tstp" => tstpOutput = true
          case "--output" =>
            val file = args0.tail.head
            outputFileName = Some(file)
            args0 = args0.tail
          case _ => throw new IllegalArgumentException(s"Unknown parameter '$hd'.")
        }
        args0 = args0.tail
        hd = args0.head
      }
      // Command
      val parsedCommand: Command = hd match {
        case "parse" => Parse
        case "reparse" => Reparse
        case "lint" => Lint
        case "transform" =>
          val param = args0.tail.head
          if (param.startsWith("--")) {
            args0 = args0.tail
            Transform(TPTP.AnnotatedFormula.FormulaType.withName(param.drop(2)))
          } else {
            throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --FOF or --THF.")
          }
        case "downgrade" =>
          val param = args0.tail.head
          if (param.startsWith("--")) {
            args0 = args0.tail
            Downgrade(TPTP.AnnotatedFormula.FormulaType.withName(param.drop(2)))
          } else {
            throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --TFF.")
          }
        case "import" =>
          val param = args0.tail.head
          if (param.startsWith("--")) {
            args0 = args0.tail
            val lang = param.drop(2) match {
              case "LRML" => leo.modules.tptputils.Import.LegalRuleML
              case _ => throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --LRML.")
            }
            Import(lang)
          } else {
            throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --LRML.")
          }
        case "export" =>
          val param = args0.tail.head
          if (param.startsWith("--")) {
            args0 = args0.tail
            val lang = param.drop(2) match {
              case "LRML" => leo.modules.tptputils.Import.LegalRuleML
              case _ => throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --LRML.")
            }
            Export(lang)
          } else {
            throw new IllegalArgumentException("Command transform expects a goal language parameter, e.g., --LRML.")
          }
        case "normalize" =>
          val param = args0.tail.head
          if (param.startsWith("--")) {
            args0 = args0.tail
            val normalform = param.drop(2) match {
              case "prenex" => Normalization.PrenexNF
              case _ => throw new IllegalArgumentException(s"Unknown goal normal form parameter: ${param.drop(2)}.")
            }
            Normalize(normalform)
          } else {
            throw new IllegalArgumentException("Command normalize expects a goal normal form, e.g., --prenex.")
          }
        case "fragment" => Fragment
        case _ => throw new IllegalArgumentException(s"Unknown command '$hd'.")
      }
      command = Some(parsedCommand)
      args0 = args0.tail
      hd = args0.head
      // Infile
      inputFileName = hd
      args0 = args0.tail
      if (args0.nonEmpty) {
        throw new IllegalArgumentException(s"Superfluous arguments supplied.")
      }
    } catch {
      case _:NoSuchElementException | _:ArrayIndexOutOfBoundsException =>
        throw new IllegalArgumentException(s"Not enough or illegal arguments supplied.")
    }
  }
}
