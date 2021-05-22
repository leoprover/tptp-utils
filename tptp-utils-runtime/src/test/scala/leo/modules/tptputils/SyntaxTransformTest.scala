package leo.modules.tptputils

import org.scalatest.funsuite.AnyFunSuite
import leo.modules.input.{TPTPParser => Parser}
import leo.modules.input.TPTPParser.TPTPParseException
import leo.datastructures.TPTP

class SyntaxTransformTest extends AnyFunSuite {
  test("test1") {
    val res = Parser.annotatedTFF("tff(a, axiom, ![X]: p(X) = a(c,d,e,e,f)).")
    println(SyntaxTransform.tffToTHF(res).pretty)
  }

  test("SYN000_1") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000_1.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
  test("SYN000_2") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000_2.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
  test("SYN000_3") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000_3.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.TFFAnnotated(_,_,_,_) => SyntaxTransform.tffToTHF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000_4") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000_4.p"))
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

  test("SYN000+1") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000+1.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.FOFAnnotated(_,_,_,_) => SyntaxTransform.fofToTFF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000+2") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000+2.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.FOFAnnotated(_,_,_,_) => SyntaxTransform.fofToTFF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000-1") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000-1.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.CNFAnnotated(_,_,_,_) => SyntaxTransform.cnfToFOF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }

  test("SYN000-2") {
    val res = Parser.problem(io.Source.fromFile("/home/lex/TPTP/Problems/SYN/SYN000-2.p"))
    val res2 = TPTP.Problem(res.includes, res.formulas.map {case f@TPTP.CNFAnnotated(_,_,_,_) => SyntaxTransform.cnfToFOF(f)} )
    println(res2.pretty)
    try {
      Parser.problem(res2.pretty)
    } catch {
      case e: TPTPParseException => fail(e.toString)
    }
  }
}
