package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.THF.Type

object Normalization {
  sealed abstract class Normalform
  case object PrenexNF extends Normalform

  final def apply(normalform: Normalform, input: TPTP.Problem): TPTP.Problem = normalform match {
    case PrenexNF => prenexNormalize(input)
  }

  @inline final def prenexNormalize(problem: TPTP.Problem): TPTP.Problem = PrenexNormalform(problem)
  @inline final def prenexNormalize(formulas: Seq[TPTP.AnnotatedFormula]): Seq[TPTP.AnnotatedFormula] = PrenexNormalform(formulas)
  @inline final def prenexNormalize(annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = PrenexNormalform.normalizeAnnotatedFormula(annotatedFormula)
  @inline final def prenexNormalizeTHF(annotatedFormula: TPTP.THFAnnotated): TPTP.THFAnnotated = PrenexNormalform.normalizeTHFAnnotatedFormula(annotatedFormula)
  @inline final def prenexNormalizeTFF(annotatedFormula: TPTP.TFFAnnotated): TPTP.TFFAnnotated = PrenexNormalform.normalizeTFFAnnotatedFormula(annotatedFormula)
  @inline final def prenexNormalizeFOF(annotatedFormula: TPTP.FOFAnnotated): TPTP.FOFAnnotated = PrenexNormalform.normalizeFOFAnnotatedFormula(annotatedFormula)

  private final object PrenexNormalform {
    def apply(problem: TPTP.Problem): TPTP.Problem = {
      TPTP.Problem(problem.includes, apply(problem.formulas), problem.formulaComments)
    }
    def apply(formulas: Seq[TPTP.AnnotatedFormula]): Seq[TPTP.AnnotatedFormula] = formulas.map(normalizeAnnotatedFormula)

    def normalizeAnnotatedFormula(annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = annotatedFormula match {
        case f@TPTP.THFAnnotated(_, _, _, _) => normalizeTHFAnnotatedFormula(f)
        case f@TPTP.TFFAnnotated(_, _, _, _) => normalizeTFFAnnotatedFormula(f)
        case f@TPTP.FOFAnnotated(_, _, _, _) => normalizeFOFAnnotatedFormula(f)
        case _ => annotatedFormula // no quantifiers in TCF, CNF, TPI
      }
    def normalizeTHFAnnotatedFormula(thfAnnotated: TPTP.THFAnnotated): TPTP.THFAnnotated = {
      import TPTP.THF
      thfAnnotated.formula match {
        case THF.Typing(_, _) => thfAnnotated
        case THF.Logical(f0) => TPTP.THFAnnotated(thfAnnotated.name, thfAnnotated.role, THF.Logical(normalizeTHFFormula(f0)), thfAnnotated.annotations)
        case THF.Sequent(_, _) => thfAnnotated
      }
    }
    def normalizeTFFAnnotatedFormula(tffAnnotated: TPTP.TFFAnnotated): TPTP.TFFAnnotated = {
      val asTHF = SyntaxTransform.tffToTHF(tffAnnotated)
      val normalizedTHF = normalizeTHFAnnotatedFormula(asTHF)
      SyntaxDowngrade.thfToTFF(normalizedTHF)
    }
    def normalizeFOFAnnotatedFormula(fofAnnotated: TPTP.FOFAnnotated): TPTP.FOFAnnotated = {
      val asTHF = SyntaxTransform.fofToTHF(fofAnnotated)
      val normalizedTHF = normalizeTHFAnnotatedFormula(asTHF)
      SyntaxDowngrade.thfToFOF(normalizedTHF)
    }

    private def invertTHFQuantifier(quantifier: TPTP.THF.Quantifier): TPTP.THF.Quantifier = {
      import TPTP.THF
      quantifier match {
        case THF.! => THF.?
        case THF.? => THF.!
        case _ => quantifier
      }
    }
    def normalizeTHFFormula(formula: TPTP.THF.Formula): TPTP.THF.Formula = {
      val normalized = normalizeTHFFormula0(formula)
      contractQuantifier(normalized)
    }
    def normalizeTHFFormula0(formula: TPTP.THF.Formula): TPTP.THF.Formula = {
      import TPTP.THF
      def shiftQuantifierOverBinaryConnective(formula: THF.Formula, connective: THF.BinaryConnective, quantifier: THF.Quantifier, variableList: Seq[(String, Type)], matrix: THF.Formula, invertQuantifiersOn: Seq[THF.BinaryConnective], placementOfQuantificationIsLeft: Boolean): THF.Formula = {
        val maybeSwitchedQuantifier = if (invertQuantifiersOn.contains(connective)) invertTHFQuantifier(quantifier) else quantifier
        val freeVarsFormula = freeVariablesTHF(formula)
        val vars = variableList.map(_._1)
        val conflictingVariables = vars.toSet intersect freeVarsFormula
        val (maybeSubstitutedMatrix, maybeSubstitutedVariableList) = if (conflictingVariables.isEmpty) (matrix, variableList) else {
          // substitute in the leftBody and rename bindings accordingly
          val substitutionForConflictingVariableNames: Map[String, String] = generateFreshVariableNamesSubstitution(conflictingVariables.toSeq, forbiddenVariableNames = vars ++ freeVarsFormula.toSeq)
          val substitutedMatrix = substituteTHF(matrix, substitutionForConflictingVariableNames)
          val substitutedVariableList = variableList.map { case (vari, ty) => (substitutionForConflictingVariableNames.withDefaultValue(vari)(vari), ty) }
          (substitutedMatrix, substitutedVariableList)
        }
        THF.QuantifiedFormula(
          maybeSwitchedQuantifier,
          maybeSubstitutedVariableList,
          normalizeTHFFormula0(
            if (placementOfQuantificationIsLeft) THF.BinaryFormula(connective, maybeSubstitutedMatrix, formula)
            else THF.BinaryFormula(connective, formula, maybeSubstitutedMatrix)
          )
        )
      }

      formula match {
        case THF.QuantifiedFormula(quantifier, variableList, body) =>
          THF.QuantifiedFormula(quantifier, variableList, normalizeTHFFormula0(body))
        case THF.UnaryFormula(connective, body) => connective match {
          case THF.~ =>
            val normalizedBody = normalizeTHFFormula0(body)
            normalizedBody match {
              case THF.QuantifiedFormula(quantifier, variableList, body) =>
                quantifier match {
                  case THF.! | THF.? =>
                    THF.QuantifiedFormula(invertTHFQuantifier(quantifier), variableList, normalizeTHFFormula0(THF.UnaryFormula(THF.~, body)))
                  case _ => THF.UnaryFormula(connective, normalizedBody)
                }
              case _ => THF.UnaryFormula(connective, normalizedBody)
            }
        }
        case THF.BinaryFormula(connective, left, right) =>
          val normalizedLeft = normalizeTHFFormula0(left)
          val normalizedRight = normalizeTHFFormula0(right)
          connective match {
            case THF.<=> | THF.<~> =>
              //rewrite as two implications and then run method on that
              val rewritten0 = THF.BinaryFormula(THF.&,
                THF.BinaryFormula(THF.Impl, normalizedLeft, normalizedRight),
                THF.BinaryFormula(THF.<=, normalizedLeft, normalizedRight)
              )
              val rewritten = if (connective == THF.<~>) THF.UnaryFormula(THF.~, rewritten0) else rewritten0
              normalizeTHFFormula0(rewritten)

            case THF.| | THF.& | THF.~| | THF.~& | THF.Impl | THF.<= => (normalizedLeft, normalizedRight) match {
              case (THF.QuantifiedFormula(leftQuantifier, leftVariableList, leftBody), _) =>
                /// invert on nots and left-implication (and right implication if right side is quantification)
                shiftQuantifierOverBinaryConnective(normalizedRight, connective, leftQuantifier, leftVariableList, leftBody, invertQuantifiersOn = Seq(THF.~|, THF.~&, THF.Impl), placementOfQuantificationIsLeft = true)
              case (_, THF.QuantifiedFormula(rightQuantifier, rightVariableList, rightBody)) =>
                shiftQuantifierOverBinaryConnective(normalizedLeft, connective, rightQuantifier, rightVariableList, rightBody, invertQuantifiersOn = Seq(THF.~|, THF.~&, THF.<=), placementOfQuantificationIsLeft = false)
              case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
            }

            case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
          }
        case _ => formula
      }
    }

    private def contractQuantifier(formula: TPTP.THF.Formula): TPTP.THF.Formula = {
      import TPTP.THF
      formula match {
        case THF.QuantifiedFormula(quantifier1, variableList1, body1) => body1 match {
          case THF.QuantifiedFormula(quantifier2, variableList2, body2) if quantifier1 == quantifier2 =>
            contractQuantifier(THF.QuantifiedFormula(quantifier1, variableList1 ++ variableList2, body2))
          case f@THF.QuantifiedFormula(_, _, _) =>
            THF.QuantifiedFormula(quantifier1, variableList1, contractQuantifier(f))
          case _ => formula
        }
        case _ => formula
      }
    }
  }
}
