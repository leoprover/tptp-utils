package leo.modules.tptputils

import org.scalatest.funsuite.AnyFunSuite
import leo.modules.input.{TPTPParser => Parser}
import leo.modules.input.TPTPParser.TPTPParseException
import leo.datastructures.TPTP

class SyntaxTransformTest extends AnyFunSuite {
  test("test1") {
    val input =
      """
        |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        |% Example 1: Basic modal reasoning
        |%
        |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        |
        |%--- logic specification
        |tff(spec, logic, ( $modal := [
        |   $constants := $rigid,
        |   $quantification := $constant,
        |   $consequence := $global,
        |   $modalities := $modal_system_S5  ] )).
        |
        |%--- does ϕ → □◇ϕ hold?
        |tff(mysterious, conjecture, ![A:$o]: (A => ($box($dia(A)))) ).""".stripMargin
    val res = Parser.problem(input)
    println(SyntaxTransform.transformProblem(TPTP.AnnotatedFormula.FormulaType.THF, res).pretty)
  }

  test("test2") {
    val input =
      """
        |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        |% Example 1: Basic modal reasoning
        |%
        |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        |
        |%--- logic specification
        |tff(spec, logic, ( $modal := [
        |   $constants := $rigid,
        |   $quantification := $constant,
        |   $consequence := $global,
        |   $modalities := $modal_system_S5  ] )).
        |
        |%--- does ϕ → □◇ϕ hold?
        |tff(mysterious, conjecture, ![A:$o]: (A => ([#a](<#b>(A)))) ).""".stripMargin
    val res = Parser.problem(input)
    println(SyntaxTransform.transformProblem(TPTP.AnnotatedFormula.FormulaType.THF, res).pretty)
  }

  test("SYN000_4 to THF") {
    val res = Parser.problem(io.Source.fromResource("SYN000_4.tptp"))
    val res2 = SyntaxTransform.transformProblem(TPTP.AnnotatedFormula.FormulaType.THF, res)
    println(res2.pretty)
  }

  test("SYN000+1 to CNF fails") {
    assertThrows[TPTPTransformException]{
      val res = Parser.problem(io.Source.fromResource("SYN000+1.tptp"))
      val res2 = SyntaxTransform.transformProblem(TPTP.AnnotatedFormula.FormulaType.CNF, res)
      println(res2.pretty)
      try {
        Parser.problem(res2.pretty)
      } catch {
        case e: TPTPParseException => fail(e.toString)
      }
    }
  }

  test("SYN000_1 to THF") {
    val res = Parser.problem(io.Source.fromResource("SYN000_1.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
  test("SYN000_2 to THF") {
    val res = Parser.problem(io.Source.fromResource("SYN000_2.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
  test("SYN000_3 to THF") {
    val res = Parser.problem(io.Source.fromResource("SYN000_3.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000_4 to THF reparse") {
    val res = Parser.problem(io.Source.fromResource("SYN000_4.tptp"))
    println(res.pretty)
    println("########################")
    println(res.formulas.map(_.toString).mkString("\n"))
    println("########################")
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    println("########################")
    println(res2.formulas.map(_.toString).mkString("\n"))
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(s"${e.line}:${e.offset} - ${e.toString}")
    }
  }

  test("SYN000+1 to THFF") {
    val res = Parser.problem(io.Source.fromResource("SYN000+1.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.FOFAnnotated(_,_,_,_) => SyntaxTransform.fofToTFF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000+2 to TFF") {
    val res = Parser.problem(io.Source.fromResource("SYN000+2.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.FOFAnnotated(_,_,_,_) => SyntaxTransform.fofToTFF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000-1 to FOF") {
    val res = Parser.problem(io.Source.fromResource("SYN000-1.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.CNFAnnotated(_,_,_,_) => SyntaxTransform.cnfToFOF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000-2 to FOF") {
    val res = Parser.problem(io.Source.fromResource("SYN000-2.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.CNFAnnotated(_,_,_,_) => SyntaxTransform.cnfToFOF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000-2 to TFF") {
    val res = Parser.problem(io.Source.fromResource("SYN000-2.tptp"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.CNFAnnotated(_,_,_,_) => SyntaxTransform.cnfToTFF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
}
