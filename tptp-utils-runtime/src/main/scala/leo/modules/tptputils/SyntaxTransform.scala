package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.FOFAnnotated

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object SyntaxTransform {
  @inline final def tffToTHF(tff: TPTP.TFFAnnotated): TPTP.THFAnnotated =
    TPTP.THFAnnotated(tff.name, tff.role, tffStatementToTHF(tff.formula), tff.annotations)
  @inline final def tffToTHF(tffs: Seq[TPTP.TFFAnnotated]): Seq[TPTP.THFAnnotated] = tffs.map(tffToTHF)
  @inline final def fofToTHF(fof: TPTP.FOFAnnotated): TPTP.THFAnnotated = tffToTHF(fofToTFF(fof))
  @inline final def fofToTHF(fofs: Seq[TPTP.FOFAnnotated]): Seq[TPTP.THFAnnotated] = fofs.map(fofToTHF)
  @inline final def cnfToTHF(cnf: TPTP.CNFAnnotated): TPTP.THFAnnotated = fofToTHF(cnfToFOF(cnf))
  @inline final def cnfToTHF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.THFAnnotated] = cnfs.map(cnfToTHF)

  @inline final def fofToTFF(fof: TPTP.FOFAnnotated): TPTP.TFFAnnotated =
    TPTP.TFFAnnotated(fof.name, fof.role, fofStatementToTFF(fof.formula), fof.annotations)
  @inline final def fofToTFF(fofs: Seq[TPTP.FOFAnnotated]): Seq[TPTP.TFFAnnotated] = fofs.map(fofToTFF)
  @inline final def cnfToTFF(cnf: TPTP.CNFAnnotated): TPTP.TFFAnnotated = fofToTFF(cnfToFOF(cnf))
  @inline final def cnfToTFF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.TFFAnnotated] = cnfs.map(cnfToTFF)

  @inline final def cnfToFOF(cnf: TPTP.CNFAnnotated): TPTP.FOFAnnotated =
    TPTP.FOFAnnotated(cnf.name, cnf.role, cnfStatementToFOF(cnf.formula), cnf.annotations)
  @inline final def cnfToFOF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.FOFAnnotated] = cnfs.map(cnfToFOF)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TFF TO THF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  private[this] final def tffStatementToTHF(statement: TPTP.TFF.Statement): TPTP.THF.Statement = {
    import TPTP.{TFF, THF}
    statement match {
      case TFF.Typing(atom, typ) => THF.Typing(atom, tffTypeToTHF(typ))
      case TFF.Logical(formula) => THF.Logical(tffLogicFormulaToTHF(formula))
    }
  }
  private[this] final def tffLogicFormulaToTHF(formula: TPTP.TFF.Formula): TPTP.THF.Formula = {
    import TPTP.{TFF, THF}
    formula match {
      case TFF.AtomicFormula(f, args) =>
        args.foldLeft[THF.Formula](THF.FunctionTerm(f, Seq.empty)) { case (expr, arg) => THF.BinaryFormula(THF.App, expr, tffTermToTHF(arg)) }
      case TFF.QuantifiedFormula(quantifier, variableList, body) =>
        val quantifier0 = quantifier match {
          case TFF.! => THF.!
          case TFF.? => THF.?
        }
        val varList0: Seq[THF.TypedVariable] = variableList.map {
          case (name, None) => (name, THF.FunctionTerm("$i", Seq.empty))
          case (name, Some(typ)) => (name, tffTypeToTHF(typ))
        }
        THF.QuantifiedFormula(quantifier0, varList0, tffLogicFormulaToTHF(body))
      case TFF.UnaryFormula(connective, body) =>
        THF.UnaryFormula(tffUnaryConnectiveToTHF(connective), tffLogicFormulaToTHF(body))
      case TFF.BinaryFormula(connective, left, right) =>
        THF.BinaryFormula(tffBinaryConnectiveToTHF(connective), tffLogicFormulaToTHF(left), tffLogicFormulaToTHF(right))
      case TFF.Equality(left, right) =>
        THF.BinaryFormula(THF.Eq, tffTermToTHF(left), tffTermToTHF(right))
      case TFF.Inequality(left, right) =>
        THF.BinaryFormula(THF.Neq, tffTermToTHF(left), tffTermToTHF(right))
      case TFF.FormulaVariable(name) => THF.Variable(name)
      case TFF.ConditionalFormula(condition, thn, els) =>
        THF.ConditionalTerm(tffLogicFormulaToTHF(condition), tffTermToTHF(thn), tffTermToTHF(els))
      case TFF.LetFormula(typing, binding, body) =>
        val typing0 = typing.map { case (name, typ) => (name, tffTypeToTHF(typ))}
        val binding0 = binding.map { case (lhs, rhs) => (tffTermToTHF(lhs), tffTermToTHF(rhs)) }
        THF.LetTerm(typing0, binding0, tffTermToTHF(body))
      case TFF.Assignment(lhs, rhs) => THF.BinaryFormula(THF.:=, tffTermToTHF(lhs), tffTermToTHF(rhs))
    }
  }
  private[this] final def tffUnaryConnectiveToTHF(conn: TPTP.TFF.UnaryConnective): TPTP.THF.UnaryConnective = {
    import TPTP.{TFF, THF}
    conn match {
      case TFF.~ => THF.~
    }
  }
  private[this] final def tffBinaryConnectiveToTHF(conn: TPTP.TFF.BinaryConnective): TPTP.THF.BinaryConnective = {
    import TPTP.{TFF, THF}
    conn match {
      case TFF.<=> => THF.<=>
      case TFF.Impl => THF.Impl
      case TFF.<= => THF.<=
      case TFF.<~> => THF.<~>
      case TFF.~| => THF.~|
      case TFF.~& => THF.~&
      case TFF.| => THF.|
      case TFF.& => THF.&
    }
  }

  private[this] final def tffTermToTHF(term: TPTP.TFF.Term): TPTP.THF.Formula = {
    import TPTP.{TFF, THF}
    term match {
      case TFF.AtomicTerm(f, args) =>
        args.foldLeft[THF.Formula](THF.FunctionTerm(f, Seq.empty)) { case (expr, arg) => THF.BinaryFormula(THF.App, expr, tffTermToTHF(arg)) }
      case TFF.Variable(name) => THF.Variable(name)
      case TFF.DistinctObject(name) => THF.DistinctObject(name)
      case TFF.NumberTerm(value) => THF.NumberTerm(value)
      case TFF.Tuple(elements) =>
        val elements0 = elements.map(tffTermToTHF)
        THF.Tuple(elements0)
      case TFF.FormulaTerm(formula) => tffLogicFormulaToTHF(formula)
    }
  }

  private[this] final def tffTypeToTHF(typ: TPTP.TFF.Type): TPTP.THF.Type = {
    import TPTP.{TFF, THF}
    typ match {
      case TFF.AtomicType(name, args) => THF.FunctionTerm(name, args.map(tffTypeToTHF))
      case TFF.MappingType(left, right) =>
        left.foldRight[THF.Formula](tffTypeToTHF(right)) { case (arg, expr) => THF.BinaryFormula(THF.FunTyConstructor, expr, tffTypeToTHF(arg)) }
      case TFF.QuantifiedType(variables, body) =>
        val varList0: Seq[THF.TypedVariable] = variables.map {
          case (name, None) => (name, THF.FunctionTerm("$tType", Seq.empty))
          case (name, Some(typ)) => (name, tffTypeToTHF(typ))
        }
        THF.QuantifiedFormula(THF.!>, varList0, tffTypeToTHF(body))
      case TFF.TypeVariable(name) => THF.Variable(name)
      case TFF.TupleType(components) => THF.Tuple(components.map(tffTypeToTHF))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // FOF TO TFF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  private[this] final def fofStatementToTFF(statement: TPTP.FOF.Statement): TPTP.TFF.Statement = ???

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // CNF TO FOF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  private[this] final def cnfStatementToFOF(statement: TPTP.CNF.Statement): TPTP.FOF.Statement = {
    import TPTP.{CNF, FOF}
    statement match {
      case CNF.Logical(formula) => FOF.Logical(cnfLogicFormulaToFOF(formula))
    }
  }

  private[this] final def cnfLogicFormulaToFOF(formula: TPTP.CNF.Formula): TPTP.FOF.Formula = {
    import TPTP.{CNF, FOF}
    formula match {
      case Seq() => FOF.AtomicFormula("$false", Seq.empty)
      case _ =>
        val transformedLiterals = formula.map(cnfLiteralToFOF)
        transformedLiterals.reduceRight(FOF.BinaryFormula(FOF.|, _, _))
    }
  }

  private[this] final def cnfLiteralToFOF(literal: TPTP.CNF.Literal): TPTP.FOF.Formula = {
    import TPTP.{CNF, FOF}
    literal match {
      case CNF.PositiveAtomic(CNF.AtomicFormula(f, args)) => FOF.AtomicFormula(f, args.map(cnfTermToFOF))
      case CNF.NegativeAtomic(CNF.AtomicFormula(f, args)) => FOF.UnaryFormula(FOF.~, FOF.AtomicFormula(f, args.map(cnfTermToFOF)))
      case CNF.Equality(left, right) => FOF.Equality(cnfTermToFOF(left), cnfTermToFOF(right))
      case CNF.Inequality(left, right) => FOF.Inequality(cnfTermToFOF(left), cnfTermToFOF(right))
    }
  }

  private[this] final def cnfTermToFOF(term: TPTP.CNF.Term): TPTP.FOF.Term = {
    import TPTP.{CNF, FOF}
    term match {
      case CNF.AtomicTerm(f, args) => FOF.AtomicTerm(f, args.map(cnfTermToFOF))
      case CNF.Variable(name) => FOF.Variable(name)
      case CNF.DistinctObject(name) => FOF.DistinctObject(name)
    }
  }
}
