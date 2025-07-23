package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.TFF

import scala.annotation.tailrec
import scala.util.matching.Regex


object Fragments {
  sealed abstract class Fragment
  case object Finite extends Fragment
  case class PrefixFragment(quantifierPrefix: Seq[QuantifierType], predicates: Seq[Int], functions: Seq[Int], equality: Boolean) extends Fragment
  case object UnspecifiedFragment extends Fragment

  final def pretty(fragment: Fragment): String = {
    fragment match {
      case Finite => "Finite"
      case PrefixFragment(quantifierPrefix, predicates, functions, equality) =>
        val quantifiersToString = quantifierPrefix.map { case E => "E" case A => "A" }.mkString
        val predicatesAsString = predicates.mkString("[", ",", "]")
        val functionsAsString = functions.mkString("[", ",", "]")
        val equalityAsString = if (equality) "=" else ""
        s"(${quantifiersToString}, $predicatesAsString, $functionsAsString)$equalityAsString"
      case UnspecifiedFragment => "UnspecifiedFragment"
    }
  }

  sealed abstract class QuantifierType
  case object E extends QuantifierType
  case object A extends QuantifierType



  sealed abstract class FragmentClass
  case object FiniteFragmentClass extends FragmentClass
  case object BernaysSchoenfinkelRamseyFragment extends FragmentClass
  case object UnknownFragmentClass extends FragmentClass

  final def getFragmentClassOfFragment(fragment: Fragment): FragmentClass = {
    fragment match {
      case Finite => FiniteFragmentClass
      case PrefixFragment(quantifierPrefix, predicates, functions, equality) =>
        if (quantifiersMatchRegex(quantifierPrefix, "E*A*".r) && functions.tail.isEmpty)
          BernaysSchoenfinkelRamseyFragment
        else UnknownFragmentClass
      case UnspecifiedFragment => UnknownFragmentClass
    }
  }
  private final def quantifiersMatchRegex(quantifiers: Seq[QuantifierType], regex: Regex): Boolean = {
    val quantifiersToString = quantifiers.map { case E => "E" case A => "A" }.mkString
    regex.matches(quantifiersToString)
  }

  final def decidableFragment(fragment: FragmentClass): Boolean = {
    fragment match {
      case BernaysSchoenfinkelRamseyFragment => true
      case _ => false
    }
  }

  def apply(problem: TPTP.Problem): Fragment = {
    if (problem.formulas.size == 1 && problem.includes.isEmpty) apply(problem.formulas.head)
    else throw new UnsupportedInputException("Fragment detection only supported for single formula inputs.")
  }

  def apply(annotatedFormula: TPTP.AnnotatedFormula): Fragment = {
    annotatedFormula match {
      case TPTP.THFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case f@TPTP.TFFAnnotated(_, _, _, _) => apply(f, implicitNormalization = true)
      case f@TPTP.FOFAnnotated(_, _, _, _) => apply(f, implicitNormalization = true)
      case TPTP.TCFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case TPTP.CNFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case TPTP.TPIAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
    }
  }
  def apply(annotatedFormula: TPTP.TFFAnnotated, implicitNormalization: Boolean): Fragment = {
    import leo.datastructures.TPTP.TFF
    val processedFormula = if (implicitNormalization) Normalization.prenexNormalize(annotatedFormula) else annotatedFormula
    processedFormula.asInstanceOf[TPTP.TFFAnnotated].formula match { // type casting is ugly, the world is bad.
      case TFF.Typing(_, _) => UnspecifiedFragment
      case TFF.Logical(formula) => analizeTFFFormula(formula)
      case TFF.Sequent(_, _) => UnspecifiedFragment
    }
  }
  def apply(annotatedFormula: TPTP.FOFAnnotated, implicitNormalization: Boolean): Fragment =
    apply(SyntaxTransform.fofToTFF(annotatedFormula), implicitNormalization)

  private def tffQuantifierToQuantifierType(quantifier: TPTP.TFF.Quantifier): QuantifierType = (quantifier: @unchecked) match {
    case TFF.! => A
    case TFF.? => E
  }
  private def analizeTFFFormula(formula: TPTP.TFF.Formula): Fragment = {
    val prefixQuants = collectPrefixQuantifier(formula)
    val (hasNestedQuants, hasEquality, fool, ncl) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical(stripPrefixQuantifier(formula))
    if (fool) UnspecifiedFragment
    else if (ncl) UnspecifiedFragment
    else {
      if (hasNestedQuants) UnspecifiedFragment //TODO: Check for separation fragments, guarded fragments, etc.
      else {
        // check for bernauys-schönfikel-ramsey-like classes
        val (predicates, functions) = listPredicateandFunctionArities(formula)
        PrefixFragment(prefixQuants, predicates, functions, hasEquality)
      }
    }
  }

  // Returns (quantifier?, equality?, fool features?, non-classical?)
  private def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical(formula: TFF.Formula): (Boolean, Boolean, Boolean, Boolean) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(formula, false, false, false, false)
  private def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(formula: TFF.Formula, qAcc: Boolean, eqAcc: Boolean, foolAcc: Boolean, nclAcc: Boolean): (Boolean, Boolean, Boolean, Boolean) = {
    if (qAcc && eqAcc && foolAcc && nclAcc) (qAcc, eqAcc, foolAcc, nclAcc)
    else {
      import TPTP.TFF
      formula match {
        case TFF.QuantifiedFormula(_, _, body) => hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(body, qAcc = true, eqAcc, foolAcc, nclAcc)
        case TFF.UnaryFormula(_, body) => hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(body, qAcc, eqAcc, foolAcc, nclAcc)
        case TFF.BinaryFormula(_, left, right) =>
          val (qAccL, eqAccL, foolAccL, nclAccL) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(left, qAcc, eqAcc, foolAcc, nclAcc)
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(right, qAcc || qAccL, eqAcc || eqAccL, foolAcc || foolAccL, nclAcc || nclAccL)
        case TFF.AtomicFormula(_, args) =>
          args.foldLeft((qAcc, eqAcc, foolAcc, nclAcc)) { case (acc, arg) =>
             hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(arg, acc._1, acc._2, acc._3, acc._4)
          }
        case TFF.ConditionalFormula(condition, thn, els) => // this is FOOL
          val (qAccC, eqAccC, foolAccC, nclAccC) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(condition, qAcc, eqAcc, foolAcc = true, nclAcc)
          val (qAccT, eqAccT, foolAccT, nclAccT) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(thn, qAccC, eqAccC, foolAccC, nclAccC)
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(els, qAccT, eqAccT, foolAccT, nclAccT)
        case TFF.LetFormula(_, binding, body) => // this is FOOL
          val (qAccB, eqAccB, foolAccB, nclAccB) = binding.map(_._2).foldLeft((qAcc, eqAcc, true, nclAcc)) { case (acc, arg) =>
            hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(arg, acc._1, acc._2, acc._3, acc._4)
          }
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(body, qAccB, eqAccB, foolAccB, nclAccB)
        case TFF.FormulaVariable(_) => // this is FOOL
          (qAcc, eqAcc, true, nclAcc)

        case TFF.Equality(left, right) =>
          val (qAccL, eqAccL, foolAccL, nclAccL) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(left, qAcc, eqAcc = true, foolAcc, nclAcc)
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(right, qAcc || qAccL, eqAccL, foolAcc || foolAccL, nclAcc || nclAccL)
        case TFF.Inequality(left, right) =>
          val (qAccL, eqAccL, foolAccL, nclAccL) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(left, qAcc, eqAcc = true, foolAcc, nclAcc)
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(right, qAcc || qAccL, eqAccL, foolAcc || foolAccL, nclAcc || nclAccL)
        case TFF.NonclassicalPolyaryFormula(_, args) =>
          args.foldLeft((qAcc, eqAcc, foolAcc, true)) { case (acc, arg) =>
            hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(arg, acc._1, acc._2, acc._3, acc._4)
          }

        case _ => (qAcc, eqAcc, foolAcc, nclAcc)
      }
    }
  }
  // Returns (quantifier?, equality?)
  private def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(term: TPTP.TFF.Term, qAcc: Boolean, eqAcc: Boolean, foolAcc: Boolean, nclAcc: Boolean): (Boolean, Boolean, Boolean, Boolean) = {
    import TPTP.TFF
    if (qAcc && eqAcc && foolAcc && nclAcc) (qAcc, eqAcc, foolAcc, nclAcc)
    else {
      term match {
        case TFF.AtomicTerm(_, args) =>
          args.foldLeft((qAcc, eqAcc, foolAcc, nclAcc)) { case (acc, arg) =>
            hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(arg, acc._1, acc._2, acc._3, acc._4)
          }
        case TFF.Tuple(elements) => // FOOL
          elements.foldLeft((qAcc, eqAcc, true, nclAcc)) { case (acc, arg) =>
            hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(arg, acc._1, acc._2, acc._3, acc._4)
          }
        case TFF.FormulaTerm(formula) => // FOOL
          hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(formula, qAcc, eqAcc, foolAcc = true, nclAcc)
        case _ => (qAcc, eqAcc, foolAcc, nclAcc)
      }
    }
  }
  private def listPredicateandFunctionArities(formula: TFF.Formula): (Seq[Int], Seq[Int]) = {
    val (preds, funcs) = listPredicateandFunctionArities0(formula, Map.empty, Map.empty)
    (toAritySeq(preds), toAritySeq(funcs))
  }
  private def listPredicateandFunctionArities0(formula: TFF.Formula, predAcc: Map[Int, Set[String]], funcAcc: Map[Int, Set[String]]): (Map[Int, Set[String]], Map[Int, Set[String]]) = {
    import TPTP.TFF
    formula match {
      case TFF.AtomicFormula(f, args) =>
        args.foldLeft((incArityCounter(predAcc, f, args.size), funcAcc)) { case (acc, arg) =>
          listPredicateandFunctionArities0(arg, acc._1, acc._2)
        }
      case TFF.QuantifiedFormula(_, _, body) => listPredicateandFunctionArities0(body, predAcc, funcAcc)
      case TFF.UnaryFormula(_, body) => listPredicateandFunctionArities0(body, predAcc, funcAcc)
      case TFF.BinaryFormula(_, left, right) =>
        val (predAccL, funcAccL) = listPredicateandFunctionArities0(left, predAcc, funcAcc)
        listPredicateandFunctionArities0(right, predAccL, funcAccL)
      case TFF.Equality(left, right) =>
        val (predAccL, funcAccL) = listPredicateandFunctionArities0(left, predAcc, funcAcc)
        listPredicateandFunctionArities0(right, predAccL, funcAccL)
      case TFF.Inequality(left, right) =>
        val (predAccL, funcAccL) = listPredicateandFunctionArities0(left, predAcc, funcAcc)
        listPredicateandFunctionArities0(right, predAccL, funcAccL)
      case TFF.NonclassicalPolyaryFormula(_, args) =>
        args.foldLeft((predAcc, funcAcc)) { case (acc, arg) =>
          listPredicateandFunctionArities0(arg, acc._1, acc._2)
        }
      case TFF.ConditionalFormula(condition, thn, els) =>
        val (predAccC, funcAccC) = listPredicateandFunctionArities0(condition, predAcc, funcAcc)
        val (predAccT, funcAccT) = listPredicateandFunctionArities0(thn, predAccC, funcAccC)
        listPredicateandFunctionArities0(els, predAccT, funcAccT)
      case TFF.LetFormula(typing, binding, body) => //FIXME: How to count here?
        listPredicateandFunctionArities0(body, predAcc, funcAcc)

      case _ => (predAcc, funcAcc)
    }
  }

  private def listPredicateandFunctionArities0(term: TFF.Term, predAcc: Map[Int, Set[String]], funcAcc: Map[Int, Set[String]]): (Map[Int, Set[String]], Map[Int, Set[String]]) = {
    import TPTP.TFF
    term match {
      case TFF.AtomicTerm(f, args) =>
        args.foldLeft((predAcc, incArityCounter(funcAcc, f, args.size))) { case (acc, arg) =>
          listPredicateandFunctionArities0(arg, acc._1, acc._2)
        }
      case TFF.Tuple(elements) =>
        elements.foldLeft((predAcc, funcAcc)) { case (acc, arg) =>
          listPredicateandFunctionArities0(arg, acc._1, acc._2)
        }
      case TFF.FormulaTerm(formula) => listPredicateandFunctionArities0(formula, predAcc, funcAcc)
      case _ => (predAcc, funcAcc)
    }
  }
  private def incArityCounter(map: Map[Int, Set[String]], symbol: String,  arity: Int): Map[Int, Set[String]] = {
    if (map.isDefinedAt(arity)) {
      val symbols = map(arity)
      map + (arity -> (symbols + symbol))
    } else map + (arity -> Set(symbol))
  }
  private def toAritySeq(arityMap: Map[Int, Set[String]]): Seq[Int] = {
    if (arityMap.isEmpty) Seq.empty
    else {
      val maximalArity = arityMap.keySet.max
      (0 until maximalArity+1).map (i => arityMap.withDefaultValue(Seq.empty).apply(i).size )
    }
  }

  @tailrec
  private def stripPrefixQuantifier(formula: TPTP.TFF.Formula): TPTP.TFF.Formula = {
    import TPTP.TFF
    formula match {
      case TFF.QuantifiedFormula(quantifier, _, body) if quantifier == TFF.! || quantifier == TFF.? =>
        stripPrefixQuantifier(body)
      case _ => formula
    }
  }
  @tailrec def collectPrefixQuantifier(formula: TPTP.TFF.Formula, accumulator: Seq[QuantifierType] = Seq.empty): Seq[QuantifierType] = {
    import TPTP.TFF
    formula match {
      case TFF.QuantifiedFormula(quantifier, _, body) if quantifier == TFF.! || quantifier == TFF.? =>
        collectPrefixQuantifier(body, accumulator :+ tffQuantifierToQuantifierType(quantifier))
      case _ => accumulator
    }
  }
}
