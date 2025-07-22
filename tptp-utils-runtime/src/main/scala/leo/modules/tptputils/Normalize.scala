package leo.modules.tptputils

import leo.datastructures.TPTP

object Normalize {

  final def prenexNormalize(problem: TPTP.Problem): TPTP.Problem = PrenexNormalform(problem)

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
    def normalizedTHFAnnotatedFormula(thfAnnotated: TPTP.THFAnnotated): TPTP.THFAnnotated = {
      import TPTP.THF
      thfAnnotated.formula match {
        case THF.Typing(_, _) => thfAnnotated
        case THF.Logical(f0) => TPTP.THFAnnotated(thfAnnotated.name, thfAnnotated.role, THF.Logical(normalizeTHFFormula(f0)), thfAnnotated.annotations)
        case THF.Sequent(_, _) => thfAnnotated
      }
    }

//    def normalizeFOFFormula(formula: TPTP.FOF.Formula): TPTP.FOF.Formula = formula match {
//      case FOF.AtomicFormula(_, _) => formula
//      case FOF.QuantifiedFormula(quantifier, variableList, body) => FOF.QuantifiedFormula(quantifier, variableList, normalizeFOFFormula(body))
//      case FOF.UnaryFormula(_, body) =>
//        val normalizedBody = normalizeFOFFormula(body)
//        normalizedBody match {
//          case FOF.QuantifiedFormula(quantifier, variableList, body) => quantifier match {
//            case FOF.! => FOF.QuantifiedFormula(FOF.?, variableList, normalizeFOFFormula(FOF.UnaryFormula(FOF.~, body)))
//            case FOF.? => FOF.QuantifiedFormula(FOF.!, variableList, normalizeFOFFormula(FOF.UnaryFormula(FOF.~, body)))
//          }
//          case _ => normalizedBody
//        }
//      case FOF.BinaryFormula(connective, left, right) =>
//        val normalizedLeft = normalizeFOFFormula(left)
//        val normalizedRight = normalizeFOFFormula(right)
//        connective match {
//        case FOF.<=> => ???
//        case FOF.Impl => ???
//        case FOF.<= => ???
//        case FOF.<~> => ???
//        case FOF.~| => ???
//        case FOF.~& => ???
//        case FOF.| => ???
//        case FOF.& => (normalizedLeft, normalizedRight) match {
//          case (FOF.QuantifiedFormula(quantifierLeft, variableListLeft, bodyLeft), r@FOF.QuantifiedFormula(quantifierRight, variableListRight, bodyRight)) =>
//            // if variable name clashes are presend, we rename in the right formula
//            val overlappingVariableNames = variableListLeft intersect variableListRight
//            if (overlappingVariableNames.isEmpty) {
//              FOF.QuantifiedFormula(quantifierLeft, variableListLeft,
//                FOF.QuantifiedFormula(quantifierRight, variableListRight,
//                  normalizeFOFFormula(FOF.BinaryFormula(connective, bodyLeft, bodyRight))))
//            } else {
//              val substitutionForConflictingVariableNames: Map[String, String] = generateFreshVariableNamesSubstitution(overlappingVariableNames, forbiddenVariableNames = variableListLeft concat variableListRight)
//              val substitutedBodyRight = substituteFOF(bodyRight, substitutionForConflictingVariableNames)
//              val substitutedVariableListRight = variableListRight.map(substitutionForConflictingVariableNames.withDefault(identity))
//              FOF.QuantifiedFormula(quantifierLeft, variableListLeft,
//                FOF.QuantifiedFormula(quantifierRight, substitutedVariableListRight,
//                  normalizeFOFFormula(FOF.BinaryFormula(connective, bodyLeft, substitutedBodyRight))))
//            }
//
//          case (FOF.QuantifiedFormula(quantifierLeft, variableListLeft, bodyLeft), _) =>
//            FOF.QuantifiedFormula(quantifierLeft, variableListLeft, normalizeFOFFormula(FOF.BinaryFormula(FOF.&, bodyLeft, normalizedRight)))
//          case (_, FOF.QuantifiedFormula(quantifierRight, variableListRight, bodyRight)) =>
//            FOF.QuantifiedFormula(quantifierRight, variableListRight, normalizeFOFFormula(FOF.BinaryFormula(FOF.&, normalizedLeft, bodyRight)))
//          case _ => FOF.BinaryFormula(FOF.&, normalizedLeft, normalizedRight)
//        }
//      }
//      case FOF.Equality(_, _) => formula
//      case FOF.Inequality(_, _) => formula
//    }
//
//    def normalizeTFFFormula(formula: TPTP.TFF.Formula): TPTP.TFF.Formula = ???

    def invertTHFQuantifier(quantifier: TPTP.THF.Quantifier): TPTP.THF.Quantifier = {
      import TPTP.THF
      quantifier match {
        case THF.! => THF.?
        case THF.? => THF.!
        case _ => quantifier
      }
    }
    def normalizeTHFFormula(formula: TPTP.THF.Formula): TPTP.THF.Formula = {
      import TPTP.THF
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
            case THF.<=> => ???
            case THF.<~> => ???

            case THF.| | THF.& | THF.~| | THF.~& | THF.Impl | THF.<= => (normalizedLeft, normalizedRight) match {
              case (THF.QuantifiedFormula(leftQuantifier, leftVariableList, leftBody), _) =>
                /// invert on nots and left-implication (and right implication if right side is quantification)
                val maybeSwitchedLeftQuantifier = if (connective == THF.~| || connective == THF.~& | connective == THF.Impl) invertTHFQuantifier(leftQuantifier) else leftQuantifier
                val freeVarsRight = freeVariablesTHF(normalizedRight)
                val leftVars = leftVariableList.map(_._1)
                val conflictingVariables = leftVars.toSet intersect freeVarsRight
                if (conflictingVariables.isEmpty) {
                  THF.QuantifiedFormula(maybeSwitchedLeftQuantifier, leftVariableList,
                    normalizeTHFFormula(
                      THF.BinaryFormula(connective,
                        leftBody,
                        normalizedRight)
                    )
                  )
                } else {
                  // substitute in the leftBody and rename bindings accordingly
                  val substitutionForConflictingVariableNames: Map[String, String] = generateFreshVariableNamesSubstitution(conflictingVariables.toSeq, forbiddenVariableNames = leftVars ++ freeVarsRight.toSeq)
                  val substitutedLeftBody = substituteTHF(leftBody, substitutionForConflictingVariableNames)
                  val substitutedLeftVariableList = leftVariableList.map { case (vari,ty) =>  (substitutionForConflictingVariableNames.withDefaultValue(vari)(vari), ty) }
                  THF.QuantifiedFormula(maybeSwitchedLeftQuantifier, substitutedLeftVariableList,
                    normalizeTHFFormula(
                      THF.BinaryFormula(connective,
                        substitutedLeftBody,
                        normalizedRight)
                    )
                  )
                }
              case (_, THF.QuantifiedFormula(rightQuantifier, rightVariableList, rightBody)) => ???
              case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
            }

            case _ => THF.BinaryFormula(connective, normalizedLeft, normalizedRight)
          }
        case _ => formula
      }
    }

  }
}
