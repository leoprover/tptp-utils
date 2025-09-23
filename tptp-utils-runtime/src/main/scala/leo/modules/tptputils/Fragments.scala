package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.TFF

import scala.annotation.tailrec
import scala.util.matching.Regex


object Fragments {
  /////////////////////////////
  // individual fragments
  ////////////////////////////
  sealed abstract class QuantifierType
  final case object E extends QuantifierType
  final case object A extends QuantifierType

  sealed abstract class Fragment
  final case object PropositionalFragment extends Fragment
  final case class PrefixFragment(quantifierPrefix: Seq[QuantifierType], predicates: Seq[Int], functions: Seq[Int], equality: Boolean) extends Fragment

  final def prettyFragment(fragment: Fragment): String = {
    fragment match {
      case PropositionalFragment => "Propositional"
      case PrefixFragment(quantifierPrefix, predicates, functions, equality) =>
        val quantifiersToString = quantifierPrefix.map { case E => "E" case A => "A" }.mkString
        val predicatesAsString = predicates.mkString("[", ",", "]")
        val functionsAsString = functions.mkString("[", ",", "]")
        val equalityAsString = if (equality) "=" else ""
        s"($quantifiersToString, $predicatesAsString, $functionsAsString)$equalityAsString"
    }
  }

  sealed abstract class FragmentClass
  final case object PropositionalFragmentClass extends FragmentClass {
    override def toString: String = "Propositional"
  }
  /** Relational first-order logic with equality with prefix quantifiers of the form E*A* */
  final case object BernaysSchoenfinkelRamseyFragmentClass extends FragmentClass {
    override def toString: String = "BernaysSchoenfinkelRamsey"
  }
  /** Relational first-order logic without equality with only unary predicate symbols. */
  final case object MonadicFirstOrderFragmentClass extends FragmentClass {
    override def toString: String = "MonadicFirstOrder"
  }
  /** Relational first-order logic with equality with only unary predicate symbols, extension of MFO */
  final case object LoewenheimFragmentClass extends FragmentClass {
    override def toString: String = "Löwenheim"
  }
  /** First-order logic without equality with only unary predicate symbols and only unary function symbols, extension of MFO */
  final case object LoebGurevichFragmentClass extends FragmentClass {
    override def toString: String = "LöbGurevich"
  }
  /** Relational first-order logic without equality with prefix quantifiers of the form E*AAE* */
  final case object GoedelKalmarSchuetteFragmentClass extends FragmentClass {
    override def toString: String = "GödelKalmárSchütte"
  }
  /** Relational first-order logic without equality with prefix quantifiers of the form E*AE* */
  final case object AckermannFragmentClass extends FragmentClass {
    override def toString: String = "Ackermann"
  }
  /** first-order logic without equality with prefix quantifiers of the form E*AE*. Extension of Ackermann */
  final case object GurevichMaslovOrevkov extends FragmentClass {
    override def toString: String = "GurevichMaslovOrevkov"
  }
  final val knownClasses: Seq[FragmentClass] = Seq(PropositionalFragmentClass,
    BernaysSchoenfinkelRamseyFragmentClass, MonadicFirstOrderFragmentClass,
    LoewenheimFragmentClass, LoebGurevichFragmentClass, GoedelKalmarSchuetteFragmentClass,
    AckermannFragmentClass, GurevichMaslovOrevkov)

  private[this] final def isSubFragmentClassOf(subFragmentClass: FragmentClass, fragmentClass: FragmentClass): Boolean = {
    (subFragmentClass, fragmentClass) match {
      case (PropositionalFragmentClass, _) => true
      case (MonadicFirstOrderFragmentClass, LoewenheimFragmentClass) => true
      case (MonadicFirstOrderFragmentClass, LoebGurevichFragmentClass) => true
      case (GurevichMaslovOrevkov, AckermannFragmentClass) => true
      case (a, b) if a == b => true
      case _ => false
    }
  }
  /////////////////////////////
  // Map fragments to classes
  ////////////////////////////
  final def getFragmentClassesOfFragment(fragment: Fragment): Seq[FragmentClass] = {
    var result: Seq[FragmentClass] =  Seq.empty
    fragment match {
      case PropositionalFragment => result = result :+ PropositionalFragmentClass
      case PrefixFragment(quantifierPrefix, predicates, functions, equality) =>
        if (quantifiersMatchRegex(quantifierPrefix, "E*A*".r) && functions.length <= 1) // constants are allowed
          result = result :+ BernaysSchoenfinkelRamseyFragmentClass
        else if (quantifiersMatchRegex(quantifierPrefix, "E*AAE*".r) && functions.length <= 1 && !equality) // constants are allowed
          result = result :+ GoedelKalmarSchuetteFragmentClass
        else if (functions.length <= 1 && predicates.length <= 2 && !equality)
          result = result :+ MonadicFirstOrderFragmentClass
        else if (functions.length <= 1 && predicates.length <= 2 && equality)
          result = result :+ LoewenheimFragmentClass
        else if (functions.length <= 2 && predicates.length <= 2 && !equality)
          result = result :+  LoebGurevichFragmentClass
        else if (quantifiersMatchRegex(quantifierPrefix, "E*AE*".r) && functions.length <= 1 && !equality)
          result = result :+  AckermannFragmentClass
        else if (quantifiersMatchRegex(quantifierPrefix, "E*AE*".r) && !equality)
          result = result :+ GurevichMaslovOrevkov
    }
    result
  }
  private[this] final def quantifiersMatchRegex(quantifiers: Seq[QuantifierType], regex: Regex): Boolean = {
    val quantifiersToString = quantifiers.map { case E => "E" case A => "A" }.mkString
    regex.matches(quantifiersToString)
  }

  /////////////////////////////
  // rest: search for fragments
  ////////////////////////////
  final def apply(problem: TPTP.Problem): (TPTP.Problem, Seq[FragmentClass]) = {
    val formulas = problem.formulas
    val formulasWithFragmentInfo = formulas.map { f =>
      val fragments = apply(f)
      (fragments._1, f.name -> fragments._2)
    }
    val comments = formulasWithFragmentInfo.map { info0 =>
      val info = info0._2
      (info._1,  Seq(TPTP.Comment(TPTP.Comment.CommentFormat.LINE, TPTP.Comment.CommentType.NORMAL, info._2.map(prettyFragment).mkString(","))))
    }.toMap
    val augmentedProblem = TPTP.Problem(problem.includes, formulasWithFragmentInfo.map(_._1), comments)
    val allFragments = formulasWithFragmentInfo.filter(_._1.role != "type").map(_._2).map(_._2)
    if (allFragments.isEmpty)
      (augmentedProblem, Seq.empty)
    else {
      val hd = allFragments.head
      val rest = allFragments.tail
      val hdClasses = hd.flatMap(getFragmentClassesOfFragment)
      val restClasses = rest.map(_.flatMap(getFragmentClassesOfFragment))
      var problemFragmentClasses: Seq[FragmentClass] = Seq.empty
      hdClasses.foreach { fragmentClass =>
        if (restClasses.forall(_.exists(c => isSubFragmentClassOf(c, fragmentClass))))
          problemFragmentClasses = problemFragmentClasses :+ fragmentClass
      }
      (augmentedProblem, problemFragmentClasses)
    }
  }

  private[this] final def updateAnnotationWithFragments(annotation: TPTP.Annotations, fragments: Seq[Fragment]): TPTP.Annotations = {
    val fragmentClasses = fragments.flatMap(getFragmentClassesOfFragment)
    val annotationEntry = fragmentClasses match {
      case Seq() => Seq(TPTP.GeneralTerm(Seq.empty, None))
      case _ => fragmentClasses.map(f => TPTP.GeneralTerm(Seq(TPTP.MetaFunctionData("fragment", Seq(TPTP.GeneralTerm(Seq(TPTP.MetaFunctionData(s"'${f.toString}'", Seq.empty)), None)))), None) )
    }
    annotation match {
      case Some((gt, list)) => list match {
        case Some(value) => Some((gt, Some(value ++ annotationEntry)))
        case None => Some((gt, Some(annotationEntry)))
      }
      case None => Some((TPTP.GeneralTerm(Seq.empty, Some(annotationEntry)), None))
    }
  }

  final def apply(annotatedFormula: TPTP.AnnotatedFormula): (TPTP.AnnotatedFormula, Seq[Fragment]) = {
    annotatedFormula match {
      case TPTP.THFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case f@TPTP.TFFAnnotated(name, role, formula, annotation) =>
        val fragment = apply(f, implicitNormalization = true)
        val updatedFormula = TPTP.TFFAnnotated(name, role, formula, updateAnnotationWithFragments(annotation, fragment))
        (updatedFormula, fragment)
      case f@TPTP.FOFAnnotated(name, role, formula, annotation) =>
        val fragment = apply(f, implicitNormalization = true)
        val updatedFormula = TPTP.FOFAnnotated(name, role, formula, updateAnnotationWithFragments(annotation, fragment))
        (updatedFormula, fragment)
      case TPTP.TCFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case TPTP.CNFAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
      case TPTP.TPIAnnotated(_, _, _, _) => throw new UnsupportedInputException("Fragment detection only supported for TFF and FOF inputs.")
    }
  }
  final def apply(annotatedFormula: TPTP.TFFAnnotated, implicitNormalization: Boolean): Seq[Fragment] = {
    import leo.datastructures.TPTP.TFF
    annotatedFormula.formula match {
      case TFF.Logical(formula) =>
        // transform to negated_conjecture with ~  first if it is a conjecture
        val annotatedFormula0 = if (annotatedFormula.role == "conjecture") {
          TPTP.TFFAnnotated(annotatedFormula.name, "negated_conjecture", TFF.Logical(TFF.UnaryFormula(TFF.~, formula)), annotatedFormula.annotations)
        } else annotatedFormula
        val processedFormula = if (implicitNormalization) Normalization.prenexNormalizeTFF(annotatedFormula0) else annotatedFormula0
        (processedFormula.formula: @unchecked) match {
          case TFF.Logical(formula0) => analyzeTFFFormula(formula0)
        }
      case _ => Seq.empty
    }
  }
  final def apply(annotatedFormula: TPTP.FOFAnnotated, implicitNormalization: Boolean): Seq[Fragment] =
    apply(SyntaxTransform.fofToTFF(annotatedFormula), implicitNormalization)

  private[this] final def tffQuantifierToQuantifierType(quantifier: TPTP.TFF.Quantifier): QuantifierType = (quantifier: @unchecked) match {
    case TFF.! => A
    case TFF.? => E
  }
  private[this] final def analyzeTFFFormula(formula: TPTP.TFF.Formula): Seq[Fragment] = {
    var result: Seq[Fragment] = Seq.empty
    val prefixQuants = collectPrefixQuantifier(formula)
    val (hasNestedQuants, hasEquality, fool, ncl) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical(stripPrefixQuantifier(formula))
    if (fool || ncl) {;} /* skip */
    else {
      // here we are in the classical FOL world.
      if (hasNestedQuants) {
        //TODO: Check for separation fragments, guarded fragments, etc.
      } else {
        // it's always a prefix class at this point
        val (predicates, functions) = listPredicateandFunctionArities(formula)
        result = result :+ PrefixFragment(prefixQuants, predicates, functions, hasEquality)
        // a more specialized prefix class: propositional
        if (prefixQuants.isEmpty && !hasEquality) result = result :+ PropositionalFragment
      }
    }
    result
  }

  // Returns (quantifier?, equality?, fool features?, non-classical?)
  private[this] final def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical(formula: TFF.Formula): (Boolean, Boolean, Boolean, Boolean) = hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(formula, qAcc = false, eqAcc = false, foolAcc = false, nclAcc = false)
  private[this] final def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(formula: TFF.Formula, qAcc: Boolean, eqAcc: Boolean, foolAcc: Boolean, nclAcc: Boolean): (Boolean, Boolean, Boolean, Boolean) = {
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
  private[this] final def hasQuantifierOrEqualityOrFOOLFeaturesOrNonclassical0(term: TPTP.TFF.Term, qAcc: Boolean, eqAcc: Boolean, foolAcc: Boolean, nclAcc: Boolean): (Boolean, Boolean, Boolean, Boolean) = {
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
  private[this] final def listPredicateandFunctionArities(formula: TFF.Formula): (Seq[Int], Seq[Int]) = {
    val (preds, funcs) = listPredicateandFunctionArities0(formula, Map.empty, Map.empty)
    (toAritySeq(preds), toAritySeq(funcs))
  }
  private[this] final def listPredicateandFunctionArities0(formula: TFF.Formula, predAcc: Map[Int, Set[String]], funcAcc: Map[Int, Set[String]]): (Map[Int, Set[String]], Map[Int, Set[String]]) = {
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

  private[this] final def listPredicateandFunctionArities0(term: TFF.Term, predAcc: Map[Int, Set[String]], funcAcc: Map[Int, Set[String]]): (Map[Int, Set[String]], Map[Int, Set[String]]) = {
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
  private[this] final def incArityCounter(map: Map[Int, Set[String]], symbol: String,  arity: Int): Map[Int, Set[String]] = {
    if (map.isDefinedAt(arity)) {
      val symbols = map(arity)
      map + (arity -> (symbols + symbol))
    } else map + (arity -> Set(symbol))
  }
  private[this] final def toAritySeq(arityMap: Map[Int, Set[String]]): Seq[Int] = {
    if (arityMap.isEmpty) Seq.empty
    else {
      val maximalArity = arityMap.keySet.max
      (0 until maximalArity+1).map (i => arityMap.withDefaultValue(Seq.empty).apply(i).size )
    }
  }

  @tailrec
  private[this] final def stripPrefixQuantifier(formula: TPTP.TFF.Formula): TPTP.TFF.Formula = {
    import TPTP.TFF
    formula match {
      case TFF.QuantifiedFormula(quantifier, _, body) if quantifier == TFF.! || quantifier == TFF.? =>
        stripPrefixQuantifier(body)
      case _ => formula
    }
  }
  @tailrec private[this] final def collectPrefixQuantifier(formula: TPTP.TFF.Formula, accumulator: Seq[QuantifierType] = Seq.empty): Seq[QuantifierType] = {
    import TPTP.TFF
    formula match {
      case TFF.QuantifiedFormula(quantifier, _, body) if quantifier == TFF.! || quantifier == TFF.? =>
        collectPrefixQuantifier(body, accumulator :+ tffQuantifierToQuantifierType(quantifier))
      case _ => accumulator
    }
  }
}
