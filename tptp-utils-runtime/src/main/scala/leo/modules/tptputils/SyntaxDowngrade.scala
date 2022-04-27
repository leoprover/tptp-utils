package leo.modules.tptputils

import leo.datastructures.TPTP

object SyntaxDowngrade {
  @inline final def thfToTFF(thf: TPTP.THFAnnotated): TPTP.TFFAnnotated =
    TPTP.TFFAnnotated(thf.name, thf.role, thfStatementToTFF(thf.formula), thf.annotations)
  @inline final def thfToTFF(thfs: Seq[TPTP.THFAnnotated]): Seq[TPTP.TFFAnnotated] = thfs.map(thfToTFF)

  @inline final def apply(goalLanguage: TPTP.AnnotatedFormula.FormulaType.FormulaType,
                          problem: TPTP.Problem): TPTP.Problem = downgradeProblem(goalLanguage, problem)

  final def downgradeProblem(goalLanguage: TPTP.AnnotatedFormula.FormulaType.FormulaType,
                          problem: TPTP.Problem): TPTP.Problem = {
    var transformedFormulas: Seq[TPTP.AnnotatedFormula] = Vector.empty
    for (formula <- problem.formulas) {
      transformedFormulas = transformedFormulas :+ downgradeAnnotatedFormula(goalLanguage, formula)
    }
    TPTP.Problem(problem.includes, transformedFormulas, problem.formulaComments)
  }

  final def downgradeAnnotatedFormula(goalLanguage: TPTP.AnnotatedFormula.FormulaType.FormulaType,
                                      annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = {
    if (isMoreGeneralThan(annotatedFormula.formulaType, goalLanguage)) {
      (annotatedFormula: @unchecked) match {
        case f@TPTP.THFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.THF => f
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => thfToTFF(f)
            case _ => // TODO
              throw new IllegalArgumentException("Currently only downgrade to TFF supported.")
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => cnfToFOF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => cnfToTCF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => f
          }
        case f@TPTP.TFFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => f
            case _ => // TODO
              throw new IllegalArgumentException("Downgrade from TFF currently not supported.")
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => ???
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => ???
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => ???
          }
        case f@TPTP.FOFAnnotated(_, _, _, _) => // TODO
          throw new IllegalArgumentException("Downgrade from FOF currently not supported.")
//          (goalLanguage: @unchecked) match {
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => cnfToFOF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => cnfToTCF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => f
//          }
        case f@TPTP.TCFAnnotated(_, _, _, _) => // TODO
          throw new IllegalArgumentException("Downgrade from TCF currently not supported.")
//          (goalLanguage: @unchecked) match {
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => cnfToTCF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => f
//          }
        case f@TPTP.CNFAnnotated(_, _, _, _) => f // No language less expressive than CNF
      }
    } else SyntaxTransform.transformAnnotatedFormula(goalLanguage, annotatedFormula)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // THF to TFF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def thfStatementToTFF(statement: TPTP.THF.Statement): TPTP.TFF.Statement = {
    import TPTP.{TFF, THF}
    statement match {
      case THF.Typing(atom, typ) => TFF.Typing(atom, thfTypeToTFF(typ))
      case THF.Sequent(lhs, rhs) => TFF.Sequent(lhs.map(thfLogicFormulaToTFFTerm), rhs.map(thfLogicFormulaToTFFTerm))
      case THF.Logical(formula) => TFF.Logical(thfLogicFormulaToTFFFormula(formula))
    }
  }

  final def thfLogicFormulaToTFFFormula(formula: TPTP.THF.Formula): TPTP.TFF.Formula = {
    import TPTP.{TFF, THF}
    formula match {
      case THF.FunctionTerm(f, args) => TFF.AtomicFormula(f, args.map(thfLogicFormulaToTFFTerm))
      case THF.QuantifiedFormula(quantifier, variableList, body) =>
        val convertedQuantifier = quantifier match {
          case THF.! => TFF.!
          case THF.? => TFF.?
          case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
        }
        val convertedVariableList = variableList.map {case (name, ty) => (name, Some(thfTypeToTFF(ty)))}
        val convertedBody = thfLogicFormulaToTFFFormula(body)
        TFF.QuantifiedFormula(convertedQuantifier, convertedVariableList, convertedBody)
      case THF.Variable(name) => TFF.FormulaVariable(name)
      case THF.UnaryFormula(connective, body) =>
        val convertedConnective = connective match {
          case THF.~ => TFF.~
        }
        TFF.UnaryFormula(convertedConnective, thfLogicFormulaToTFFFormula(body))
      case THF.BinaryFormula(THF.App, left, right) =>
        left match {
          case THF.ConnectiveTerm(conn) =>
            conn match {
              case THF.NonclassicalLongOperator(name, params) => ???
              case THF.NonclassicalBox(idx) => ???
              case THF.NonclassicalDiamond(idx) => ???
              case THF.NonclassicalCone(idx) => ???
            }
          case _ => ???
        }
        // TODO
        ???
      case THF.BinaryFormula(connective, left, right) =>
        connective match {
          case THF.Eq =>
            TFF.Equality(thfLogicFormulaToTFFTerm(left), thfLogicFormulaToTFFTerm(right))
          case THF.Neq =>
            TFF.Inequality(thfLogicFormulaToTFFTerm(left), thfLogicFormulaToTFFTerm(right))
          case THF.== =>
            val convertedLeft = thfLogicFormulaToTFFTerm(left)
            convertedLeft match {
              case f@TFF.AtomicTerm(_, _) =>
                val convertedRight = thfLogicFormulaToTFFTerm(right)
                TFF.MetaIdentity(f, convertedRight)
              case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
            }
          case THF.:= =>
            val convertedLeft = thfLogicFormulaToTFFTerm(left)
            convertedLeft match {
              case f@TFF.AtomicTerm(_, _) =>
                val convertedRight = thfLogicFormulaToTFFTerm(right)
                TFF.Assignment(f, convertedRight)
              case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
            }
          case _ =>
            val convertedConnective = connective match {
              case THF.<=> => TFF.<=>
              case THF.Impl => TFF.Impl
              case THF.<= => TFF.<=
              case THF.<~> => TFF.<~>
              case THF.~| => TFF.~|
              case THF.~& => TFF.~&
              case THF.| => TFF.|
              case THF.& => TFF.&
              case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
            }
            TFF.BinaryFormula(convertedConnective, thfLogicFormulaToTFFFormula(left), thfLogicFormulaToTFFFormula(right))
        }
      case THF.ConditionalTerm(condition, thn, els) =>
        TFF.ConditionalFormula(thfLogicFormulaToTFFFormula(condition), thfLogicFormulaToTFFTerm(thn), thfLogicFormulaToTFFTerm(els))
      case THF.LetTerm(typing, binding, body) =>
        val convertedTyping = typing.map { case (name, typ) => (name, thfTypeToTFF(typ))}
        val convertedBinding = binding.map { case (lhs, rhs) => (thfLogicFormulaToTFFTerm(lhs), thfLogicFormulaToTFFTerm(rhs)) }
        TFF.LetFormula(convertedTyping, convertedBinding, thfLogicFormulaToTFFTerm(body))
      case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
    }
  }

  final def thfLogicFormulaToTFFTerm(formula: TPTP.THF.Formula): TPTP.TFF.Term = ???

  final def thfTypeToTFF(typ: TPTP.THF.Type): TPTP.TFF.Type = {
    import TPTP.{THF, TFF}
    typ match {
      case THF.FunctionTerm(f, args) =>
        val convertedArgs = args.map { arg =>
          val res = thfTypeToTFF(arg)
          res match {
            case TFF.AtomicType(_, _) => res
            case _ => throw new IllegalArgumentException(s"Unsupported type in downgrade: ${typ.pretty}")
          }
        }
        TFF.AtomicType(f, convertedArgs)
      case f@THF.BinaryFormula(THF.FunTyConstructor, _, _) =>
        val mappingTypes = collectMappingTypes(f, Seq.empty)
        val convertedArgumentTypes = mappingTypes._1.map { arg =>
          val res = thfTypeToTFF(arg)
          res match {
            case TFF.AtomicType(_, _) | TFF.TypeVariable(_) | TFF.TupleType(_) => res
            case _ => throw new IllegalArgumentException(s"Unsupported type in downgrade: ${typ.pretty}")
          }
        }
        val convertedResultType = thfTypeToTFF(mappingTypes._2)
        convertedResultType match {
          case TFF.AtomicType(_, _) | TFF.TypeVariable(_) | TFF.TupleType(_) =>
            TFF.MappingType(convertedArgumentTypes, convertedResultType)
          case _ => throw new IllegalArgumentException(s"Unsupported type in downgrade: ${typ.pretty}")
        }
      case THF.QuantifiedFormula(THF.!>, variableList, body) =>
        val convertedVariableList = variableList.map {case (name, ty) => (name, Some(thfTypeToTFF(ty)))}
        TFF.QuantifiedType(convertedVariableList, thfTypeToTFF(body))
      case THF.Variable(name) => TFF.TypeVariable(name)
      case THF.Tuple(elements) => TFF.TupleType(elements.map(thfTypeToTFF))
      case _ => throw new IllegalArgumentException(s"Unsupported type in downgrade: ${typ.pretty}")
    }
  }
  private final def collectMappingTypes(ty: TPTP.THF.BinaryFormula, acc: Seq[TPTP.THF.Type]): (Seq[TPTP.THF.Type], TPTP.THF.Type) = {
    import TPTP.THF
    ty.right match {
      case f@THF.BinaryFormula(THF.FunTyConstructor, _, _) => collectMappingTypes(f, acc :+ ty.left)
      case _ => (acc :+ ty.left, ty.right)
    }
  }

}
