package leo.modules

import leo.datastructures.TPTP
import leo.datastructures.TPTP.AnnotatedFormula.FormulaType
import leo.modules.input.TPTPParser

import java.io.FileNotFoundException
import java.nio.file.Path
import scala.io.Source

package object tptputils {
  class TPTPTransformException(message: String) extends RuntimeException(message)
  class UnsupportedInputException(message: String) extends RuntimeException(message)

  final def parseTPTPFileWithoutIncludes(path: Path): TPTP.Problem = {
    if (path.toString == "-") {
      TPTPParser.problem(Source.stdin)
    } else {
      TPTPParser.problem(Source.fromFile(path.toFile))
    }
  }
  final def parseTPTPFileWithIncludes(path: Path, tptpHomeDirectory: Option[String]): TPTP.Problem = {
    val includesAlreadyRead: collection.mutable.Set[Path] = collection.mutable.Set.empty

    def parseTPTPFile0(file: Source, filepath: Path): TPTP.Problem = {
      includesAlreadyRead.addOne(filepath.normalize())
      val problem = TPTPParser.problem(file)
      val recursivelyParsedIncludes = problem.includes.map { case (include, _) =>
        val includePath = filepath.getParent.resolve(include)
        if (includePath.toFile.exists()) {
          parseTPTPFile0(Source.fromFile(includePath.toFile), includePath)
        } else {
          tptpHomeDirectory match {
            case Some(dir) =>
              val defaultincludePath = Path.of(dir).resolve(include)
              parseTPTPFile0(Source.fromFile(defaultincludePath.toFile), defaultincludePath)
            case None => throw new FileNotFoundException(s"Include '${filepath.toString}' not found. Did you forget to define the TPTP environment variable?")
          }
        }
      }
      recursivelyParsedIncludes.foldRight(problem) { case (parsedInclude, acc) =>
        TPTP.Problem(Seq.empty, parsedInclude.formulas ++ acc.formulas, parsedInclude.formulaComments concat acc.formulaComments)
      }
    }

    if (path.toString == "-") {
      parseTPTPFile0(Source.stdin, Path.of(sys.props("user.dir")))
    } else {
      parseTPTPFile0(Source.fromFile(path.toFile), path)
    }
  }


  /** Returns true iff `role` does not contain a subrole (i.e., role followed by dash followed by general term). */
  final def isSimpleRole(role: String): Boolean = !role.contains("-")
  /** Returns true iff `role` does contain a subrole (i.e., role followed by dash followed by general term). */
  final def isComplexRole(role: String): Boolean = !isSimpleRole(role)
  /** Returns None iff `role` is a simple role; otherwise the subrole part of the role. */
  final def getSubrole(role: String): Option[String] = if (isComplexRole(role)) {
    Some(roleSplit(role)._2.tail)
  } else None
  /** Returns the non-subrole part of the role. If no subrole-part exists, the role itself it returned. */
  final def toSimpleRole(role: String): String = roleSplit(role)._1
  // roleSplit("<prefix>-<suffix>") gives ("<prefix>", "-<suffix>"), i.e., the dash is still in there.
  private[this] final def roleSplit(role: String): (String, String) = role.span(_ != '-')

  final def isMoreGeneralThan(a: FormulaType.FormulaType, b: FormulaType.FormulaType): Boolean = {
    import FormulaType._
    a match {
      case THF => b match {
        case TPI => false
        case _ => true
      }
      case TFF => b match {
        case THF | TPI => false
        case _ => true
      }
      case FOF => b match {
        case FOF | CNF => true
        case _ => false
      }
      case TCF => b match {
        case TCF | CNF => true
        case _ => false
      }
      case CNF => b match {
        case CNF => true
        case _ => false
      }
      case TPI => false
    }
  }

//  final def freeVariablesFOF(fofFormula: TPTP.FOF.Formula): Set[String] = ???
//  final def freeVariablesTFF(tffFormula: TPTP.TFF.Formula): Set[String] = ???
  final def freeVariablesTHF(thfFormula: TPTP.THF.Formula): Set[String] = freeVariablesTHF0(thfFormula, fvs = Set.empty)
  final private def freeVariablesTHF0(thfFormula: TPTP.THF.Formula, fvs: Set[String]): Set[String] = {
    import TPTP.THF
    thfFormula match {
      case THF.FunctionTerm(f, args) =>
        args.flatMap(freeVariablesTHF0(_, fvs)).toSet
      case THF.QuantifiedFormula(_, variableList, body) =>
        freeVariablesTHF0(body, fvs) -- variableList.map(_._1).toSet
      case THF.Variable(name) => fvs + name
      case THF.UnaryFormula(_, body) => freeVariablesTHF0(body, fvs)
      case THF.BinaryFormula(_, left, right) =>
        freeVariablesTHF0(left, fvs) ++ freeVariablesTHF0(right, fvs)
      case THF.Tuple(elements) => elements.flatMap(freeVariablesTHF0(_, fvs)).toSet
      case THF.ConditionalTerm(condition, thn, els) =>
        freeVariablesTHF0(condition, fvs) ++ freeVariablesTHF0(thn, fvs) ++ freeVariablesTHF0(els, fvs)
      case THF.LetTerm(typing, _, body) =>
        freeVariablesTHF0(body, fvs) -- typing.keySet
      case THF.NonclassicalPolyaryFormula(_, args) =>
        args.flatMap(freeVariablesTHF0(_, fvs)).toSet
      case _ => Set.empty
    }
  }

  /** Replace free occurrences of variables by other variables as given by `substitution`. */
  final def substituteFOF(fofFormula: TPTP.FOF.Formula, substitution: Map[String, String]): TPTP.FOF.Formula = substituteFOF0(fofFormula, substitution, boundVars = Set.empty)
  final def substituteFOF0(fofFormula: TPTP.FOF.Formula, substitution: Map[String, String], boundVars: Set[String]): TPTP.FOF.Formula = {
    import TPTP.FOF
    fofFormula match {
      case FOF.AtomicFormula(f, args) => FOF.AtomicFormula(f, args.map(substituteFOFTerm0(_, substitution, boundVars)))
      case FOF.QuantifiedFormula(quantifier, variableList, body) =>
        FOF.QuantifiedFormula(quantifier, variableList, substituteFOF0(body, substitution, boundVars ++ variableList))
      case FOF.UnaryFormula(connective, body) => FOF.UnaryFormula(connective, substituteFOF0(body, substitution, boundVars))
      case FOF.BinaryFormula(connective, left, right) => FOF.BinaryFormula(connective, substituteFOF0(left, substitution, boundVars), substituteFOF0(right, substitution, boundVars))
      case FOF.Equality(left, right) => FOF.Equality(substituteFOFTerm0(left, substitution, boundVars), substituteFOFTerm0(right, substitution, boundVars))
      case FOF.Inequality(left, right) => FOF.Inequality(substituteFOFTerm0(left, substitution, boundVars), substituteFOFTerm0(right, substitution, boundVars))
    }
  }
  final def substituteFOFTerm0(fofTerm: TPTP.FOF.Term, substitution: Map[String, String], boundVars: Set[String]): TPTP.FOF.Term = {
    import TPTP.FOF
    fofTerm match {
      case FOF.AtomicTerm(f, args) => FOF.AtomicTerm(f, args.map(substituteFOFTerm0(_, substitution, boundVars)))
      case FOF.Variable(name) if !boundVars.contains(name) => FOF.Variable(substitution.withDefaultValue(name)(name))
      case _ => fofTerm
    }
  }

  final def substituteTHF(thfFormula: TPTP.THF.Formula, substitution: Map[String, String]): TPTP.THF.Formula = substituteTHF0(thfFormula, substitution, boundVars = Set.empty)
  final def substituteTHF0(thfFormula: TPTP.THF.Formula, substitution: Map[String, String], boundVars: Set[String]): TPTP.THF.Formula = {
    import TPTP.THF
    thfFormula match {
      case THF.FunctionTerm(f, args) => THF.FunctionTerm(f, args.map(substituteTHF0(_, substitution, boundVars)))
      case THF.QuantifiedFormula(quantifier, variableList, body) =>
        THF.QuantifiedFormula(quantifier, variableList, substituteTHF0(body, substitution, boundVars ++ variableList.map(_._1).toSet))
      case THF.Variable(name) if !boundVars.contains(name) => THF.Variable(substitution.withDefaultValue(name)(name))
      case THF.UnaryFormula(connective, body) =>
        THF.UnaryFormula(connective, substituteTHF0(body, substitution, boundVars))
      case THF.BinaryFormula(connective, left, right) =>
        THF.BinaryFormula(connective, substituteTHF0(left, substitution, boundVars), substituteTHF0(right, substitution, boundVars))
      case THF.Tuple(elements) =>
        THF.Tuple(elements.map(substituteTHF0(_, substitution, boundVars)))
      case THF.ConditionalTerm(condition, thn, els) =>
        THF.ConditionalTerm(substituteTHF0(condition, substitution, boundVars), substituteTHF0(thn, substitution, boundVars), substituteTHF0(els, substitution, boundVars))
      case THF.LetTerm(typing, binding, body) =>
        THF.LetTerm(typing, binding, substituteTHF0(body, substitution, boundVars ++ typing.keySet))
      case THF.NonclassicalPolyaryFormula(connective, args) =>
        THF.NonclassicalPolyaryFormula(connective, args.map(substituteTHF0(_, substitution, boundVars)))
      case _ => thfFormula
    }
  }

  final def generateFreshVariableNamesSubstitution(newNamesFor: Seq[String], forbiddenVariableNames: Seq[String]): Map[String, String] = {
    var substitution: Map[String, String] = Map.empty
    var forbiddenVariableNames0 = forbiddenVariableNames
    newNamesFor foreach { vari =>
      val newNameForVari = generteFreshVariableName(vari, forbiddenVariableNames0)
      substitution = substitution + (vari -> newNameForVari)
      forbiddenVariableNames0 = forbiddenVariableNames0 :+ newNameForVari
    }
    substitution
  }

  final def generteFreshVariableName(newNameFor: String, forbiddenVariableNames: Seq[String]): String = {
    var counter = 0
    def varName(): String = s"$newNameFor$counter"
    while (forbiddenVariableNames contains varName()) { counter = counter + 1 }
    varName()
  }

  import scala.io.Source
  import java.io.InputStream
  import java.nio.charset.Charset
  import java.nio.CharBuffer
  import java.nio.ByteBuffer

  /**
   * A SourceInputStream obtains input bytes
   * from a scala.io.Source and encodes obtained character stream
   * into a stream of raw bytes using given encoding.
   *
   * @constructor create a new instance of SourceInputStream.
   * @param source underlying scala.io.Source.
   * @param encoding encoding, that will be used to transform character stream into a stream of bytes.
   * @see scala.io.Source
   * @see java.io.InputStream
   */
  class SourceInputStream(source: Source, encoding: String = "UTF-8") extends InputStream {

    private val encoder = Charset.forName(encoding).newEncoder()
    private var buffer = Array.ofDim[Byte](1024 * 16)
    private var bufferEnd = 0
    private var bufferPos = 0

    /**
     * Reads a byte of data from underlying scala.io.Source.
     *
     * @return     the next byte of data, or -1 if the end of the
     *             file is reached.
     */
    override def read: Int = {

      if (availableBytes < 1) {
        readNextChunk()
      }

      if (availableBytes < 1) return -1
      else {
        val result = buffer(bufferPos)
        bufferPos = math.min(bufferPos + 1, bufferEnd)
        result
      }

    }

    /**
     * Reads a subarray as a sequence of bytes.
     * @param b the data to be written
     * @param off the start offset in the data
     * @param len the number of bytes that are written
     */
    override def read(b: Array[Byte], off: Int, len: Int): Int = {

      require(off >= 0, "argument off of the %s.read should be >= 0".format(getClass().getName()))

      require(len > 0, "argument len of the %s.read should be > 0".format(getClass().getName()))

      val bytesNeeded = math.min(len, b.size - off)

      if (buffer.size < len * 2) resizeBuffer(len * 2)

      if (availableBytes < 1) {
        readNextChunk()
      }

      if (availableBytes < 1) -1
      else {
        if (availableBytes < bytesNeeded) {
          val firstHalfLength = availableBytes
          System.arraycopy(buffer, bufferPos, b, off, math.max(firstHalfLength, 0))
          readNextChunk()
          if (availableBytes < 0) firstHalfLength
          else {
            val secondHalfLength = math.min(availableBytes, bytesNeeded - firstHalfLength)
            System.arraycopy(buffer, bufferPos, b, off + firstHalfLength, secondHalfLength)
            bufferPos += secondHalfLength
            math.max(firstHalfLength + secondHalfLength, -1)
          }
        } else {
          System.arraycopy(buffer, bufferPos, b, off, bytesNeeded)
          bufferPos = math.min(bufferPos + bytesNeeded, bufferEnd)
          bytesNeeded
        }
      }
    }

    private def resizeBuffer(len: Int): Unit = {
      val newBuffer = Array.ofDim[Byte](len * 2)
      buffer.copyToArray(newBuffer)
      buffer = newBuffer
    }

    private def availableBytes: Int = bufferEnd - bufferPos

    private def readNextChunk(): Unit = {
      if (source.hasNext) {
        val chars = source.take(buffer.length / 2).toArray
        if (chars.length < 1) bufferEnd = bufferPos - 1
        else {
          bufferEnd = charArray2ByteArray(chars, buffer)
          bufferPos = 0
        }
      } else {
        bufferEnd = bufferPos - 1
      }
    }

    private def charArray2ByteArray(chars: Array[Char], bytes: Array[Byte]): Int = {
      val bb = ByteBuffer.wrap(bytes, 0, bytes.size)
      var cr = encoder.encode(CharBuffer.wrap(chars, 0, chars.length), bb, true)
      if (!cr.isUnderflow())
        cr.throwException();
      cr = encoder.flush(bb);
      if (!cr.isUnderflow())
        cr.throwException();
      encoder.reset()
      bb.position()
    }

  }

}
