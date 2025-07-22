package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.THF.Type

object Normalize {

  @inline final def prenexNormalize(problem: TPTP.Problem): TPTP.Problem = PrenexNormalform(problem)
  @inline final def prenexNormalize(formulas: Seq[TPTP.AnnotatedFormula]): Seq[TPTP.AnnotatedFormula] = PrenexNormalform(formulas)
  @inline final def prenexNormalize(annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = PrenexNormalform.normalizeAnnotatedFormula(annotatedFormula)

  private final object PrenexNormalform {
    def apply(problem: TPTP.Problem): TPTP.Problem = {
      TPTP.Problem(problem.includes, apply(problem.formulas), problem.formulaComments)
    }
    def apply(formulas: Seq[TPTP.AnnotatedFormula]): Seq[TPTP.AnnotatedFormula] = formulas.map(normalizeAnnotatedFormula)

    def normalizeAnnotatedFormula(annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = {
      import TPTP.THF
      annotatedFormula match {
        case f@TPTP.THFAnnotated(name, role, formula, annotations) => formula match {
          case THF.Typing(_, _) => f
          case THF.Logical(f0) => TPTP.THFAnnotated(name, role, THF.Logical(normalizeTHFFormula(f0)), annotations)
          case THF.Sequent(_, _) => f
        }
        case f@TPTP.TFFAnnotated(_, _, _, _) =>
          val asTHF = SyntaxTransform.tffToTHF(f)
          val normalizedTHF = normalizedTHFAnnotatedFormula(asTHF)
          SyntaxDowngrade.thfToTFF(normalizedTHF)
        case f@TPTP.FOFAnnotated(_, _, _, _) =>
          val asTHF = SyntaxTransform.fofToTHF(f)
          val normalizedTHF = normalizedTHFAnnotatedFormula(asTHF)
          SyntaxDowngrade.thfToFOF(normalizedTHF)

        case f@TPTP.TCFAnnotated(_, _, _, _) => f
        case f@TPTP.CNFAnnotated(_, _, _, _) => f
        case f@TPTP.TPIAnnotated(_, _, _, _) => f
      }
    }
    private def normalizedTHFAnnotatedFormula(thfAnnotated: TPTP.THFAnnotated): TPTP.THFAnnotated = {
      import TPTP.THF
      thfAnnotated.formula match {
        case THF.Typing(_, _) => thfAnnotated
        case THF.Logical(f0) => TPTP.THFAnnotated(thfAnnotated.name, thfAnnotated.role, THF.Logical(normalizeTHFFormula(f0)), thfAnnotated.annotations)
        case THF.Sequent(_, _) => thfAnnotated
      }
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
      import TPTP.THF
      def shiftQuantifierOverBinaryConnective(formula: THF.Formula, connective: THF.BinaryConnective, quantifier: THF.Quantifier, variableList: Seq[(String, Type)], matrix: THF.Formula, invertQuantifiersOn: Seq[THF.BinaryConnective], placementOfQuantificationIsLeft: Boolean): THF.Formula = {
        val maybeSwitchedQuantifier = if (invertQuantifiersOn.contains(connective)) invertTHFQuantifier(quantifier) else quantifier
        val freeVarsFormula = freeVariablesTHF(formula)
        val vars = variableList.map(_._1)
        val conflictingVariables = vars.toSet intersect freeVarsFormula
        if (conflictingVariables.isEmpty) {
          THF.QuantifiedFormula(maybeSwitchedQuantifier, variableList,
            normalizeTHFFormula(
              if (placementOfQuantificationIsLeft)
                THF.BinaryFormula(connective,
                  matrix,
                  formula
                )
              else
                THF.BinaryFormula(connective,
                  formula,
                  matrix
                )
            )
          )
        } else {
          // substitute in the leftBody and rename bindings accordingly
          val substitutionForConflictingVariableNames: Map[String, String] = generateFreshVariableNamesSubstitution(conflictingVariables.toSeq, forbiddenVariableNames = vars ++ freeVarsFormula.toSeq)
          val substitutedMatrix = substituteTHF(matrix, substitutionForConflictingVariableNames)
          val substitutedVariableList = variableList.map { case (vari, ty) => (substitutionForConflictingVariableNames.withDefaultValue(vari)(vari), ty) }
          THF.QuantifiedFormula(maybeSwitchedQuantifier, substitutedVariableList,
            normalizeTHFFormula(
              THF.BinaryFormula(connective,
                substitutedMatrix,
                formula)
            )
          )
        }
      }

      formula match {
        case THF.QuantifiedFormula(quantifier, variableList, body) =>
          THF.QuantifiedFormula(quantifier, variableList, normalizeTHFFormula(body))
        case THF.UnaryFormula(connective, body) => connective match {
          case THF.~ =>
            val normalizedBody = normalizeTHFFormula(body)
            normalizedBody match {
              case THF.QuantifiedFormula(quantifier, variableList, body) =>
                quantifier match {
                  case THF.! | THF.? =>
                    THF.QuantifiedFormula(invertTHFQuantifier(quantifier), variableList, normalizeTHFFormula(THF.UnaryFormula(THF.~, body)))
                  case _ => THF.UnaryFormula(connective, normalizedBody)
                }
              case _ => THF.UnaryFormula(connective, normalizedBody)
            }
        }
        case THF.BinaryFormula(connective, left, right) =>
          val normalizedLeft = normalizeTHFFormula(left)
          val normalizedRight = normalizeTHFFormula(right)
          connective match {
            case THF.<=> | THF.<~> =>
              //rewrite as two implications and then run method on that
              val rewritten0 = THF.BinaryFormula(THF.&,
                THF.BinaryFormula(THF.Impl, normalizedLeft, normalizedRight),
                THF.BinaryFormula(THF.<=, normalizedLeft, normalizedRight)
              )
              val rewritten = if (connective == THF.<~>) THF.UnaryFormula(THF.~, rewritten0) else rewritten0
              normalizeTHFFormula(rewritten)

            case THF.| | THF.& | THF.~| | THF.~& | THF.Impl | THF.<= => (normalizedLeft, normalizedRight) match {
              case (THF.QuantifiedFormula(leftQuantifier, leftVariableList, leftBody), _) =>
                /// invert on nots and left-implication (and right implication if right side is quantification)
                shiftQuantifierOverBinaryConnective(normalizedRight, connective, leftQuantifier, leftVariableList, leftBody, invertQuantifiersOn = Seq(THF.~|, THF.~&, THF.Impl))
              case (_, THF.QuantifiedFormula(rightQuantifier, rightVariableList, rightBody)) =>
                shiftQuantifierOverBinaryConnective(normalizedLeft, connective, rightQuantifier, rightVariableList, rightBody, invertQuantifiersOn = Seq(THF.~|, THF.~&, THF.<=))
              case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
            }

            case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
          }
        case _ => formula
      }
    }

  }
}
