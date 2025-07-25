package leo.modules.tptputils

import leo.datastructures.TPTP

import scala.annotation.tailrec

object SyntaxDowngrade {
  @inline final def thfToTFF(thf: TPTP.THFAnnotated): TPTP.TFFAnnotated =
    TPTP.TFFAnnotated(thf.name, thf.role, thfStatementToTFF(thf.formula), thf.annotations)
  @inline final def thfToTFF(thfs: Seq[TPTP.THFAnnotated]): Seq[TPTP.TFFAnnotated] = thfs.map(thfToTFF)

  @inline final def tffToFOF(tff: TPTP.TFFAnnotated): TPTP.FOFAnnotated =
    TPTP.FOFAnnotated(tff.name, tff.role, tffStatementToFOF(tff.formula), tff.annotations)
  @inline final def tffToFOF(tffs: Seq[TPTP.TFFAnnotated]): Seq[TPTP.FOFAnnotated] = tffs.map(tffToFOF)

  @inline final def thfToFOF(thf: TPTP.THFAnnotated): TPTP.FOFAnnotated = tffToFOF(thfToTFF(thf))
  @inline final def thfToFOF(thfs: Seq[TPTP.THFAnnotated]): Seq[TPTP.FOFAnnotated] = thfs.map(thfToFOF)

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
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => thfToFOF(f)
            case _ => // TODO
              throw new IllegalArgumentException("Downgrade from THF currently only to TFF/FOF supported.")
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => thfToFOF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => thfoTCF(f)
//            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => f
          }
        case f@TPTP.TFFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => f
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => tffToFOF(f)
            case _ => // TODO
              throw new IllegalArgumentException("Downgrade for TFF currently only to FOF supported.")
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
      case f@THF.BinaryFormula(THF.App, _, _) =>
        val (function, arguments) = collectApplicationArguments(f, Seq.empty)

        function match {
          case THF.FunctionTerm(f, Seq()) => TFF.AtomicFormula(f, arguments.map(thfLogicFormulaToTFFTerm))
          case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
        }
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
      case THF.NonclassicalPolyaryFormula(connective, args) =>
        val convertedConnective = connective match {
          case THF.NonclassicalLongOperator(name, idx, params) =>
            val convertedIdx = idx.map(thfLogicFormulaToTFFTerm)
            val convertedParams = params.map {
              case (l, r) => (thfLogicFormulaToTFFTerm(l), thfLogicFormulaToTFFTerm(r))
            }
            TFF.NonclassicalLongOperator(name, convertedIdx, convertedParams)
          case THF.NonclassicalBox(idx) => TFF.NonclassicalBox(idx.map(thfLogicFormulaToTFFTerm))
          case THF.NonclassicalDiamond(idx) => TFF.NonclassicalDiamond(idx.map(thfLogicFormulaToTFFTerm))
          case THF.NonclassicalCone(idx) => TFF.NonclassicalCone(idx.map(thfLogicFormulaToTFFTerm))
          case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
        }
        TFF.NonclassicalPolyaryFormula(convertedConnective, args.map(thfLogicFormulaToTFFFormula))
      case _ => throw new IllegalArgumentException(s"Unsupported formula in downgrade: ${formula.pretty}")
    }
  }
  @tailrec  final def collectApplicationArguments(term: TPTP.THF.BinaryFormula, acc: Seq[TPTP.THF.Formula]): (TPTP.THF.Formula, Seq[TPTP.THF.Formula]) = {
    import TPTP.THF
    term.left match {
      case f@THF.BinaryFormula(THF.App, _, _) => collectApplicationArguments(f, term.right +: acc)
      case _ => (term.left, term.right +: acc)
    }
  }

  final def thfLogicFormulaToTFFTerm(term: TPTP.THF.Formula): TPTP.TFF.Term = {
    import TPTP.{THF, TFF}
    term match {
      case THF.FunctionTerm(f, args) => TFF.AtomicTerm(f, args.map(thfLogicFormulaToTFFTerm))
      case THF.Variable(name) => TFF.Variable(name)
      case f@THF.BinaryFormula(THF.App, _, _) =>
        val (function, arguments) = collectApplicationArguments(f, Seq.empty)
        function match {
          case THF.FunctionTerm(f, Seq()) => TFF.AtomicTerm(f, arguments.map(thfLogicFormulaToTFFTerm))
          case _ => throw new IllegalArgumentException(s"Unsupported term in downgrade: ${term.pretty}")
        }

      case THF.QuantifiedFormula(_, _, _) | THF.UnaryFormula(_, _) | THF.BinaryFormula(_, _, _) | THF.ConditionalTerm(_, _, _) | THF.LetTerm(_, _, _) =>
        TFF.FormulaTerm(thfLogicFormulaToTFFFormula(term))

      case THF.Tuple(elements) => TFF.Tuple(elements.map(thfLogicFormulaToTFFTerm))
      case THF.DistinctObject(name) => TFF.DistinctObject(name)
      case THF.NumberTerm(value) => TFF.NumberTerm(value)

      case _ => throw new IllegalArgumentException(s"Unsupported term in downgrade: ${term.pretty}")

    }
  }

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
  @tailrec  final def collectMappingTypes(ty: TPTP.THF.BinaryFormula, acc: Seq[TPTP.THF.Type]): (Seq[TPTP.THF.Type], TPTP.THF.Type) = {
    import TPTP.THF
    ty.right match {
      case f@THF.BinaryFormula(THF.FunTyConstructor, _, _) => collectMappingTypes(f, acc :+ ty.left)
      case _ => (acc :+ ty.left, ty.right)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TFF to FOF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def tffStatementToFOF(statement: TPTP.TFF.Statement): TPTP.FOF.Statement = {
    import TPTP.TFF
    statement match {
      case TFF.Typing(_, _) => throw new IllegalArgumentException(s"Unsupported expression in downgrade: ${statement.pretty}")
      case TFF.Logical(formula) => TPTP.FOF.Logical(tffLogicFormulaToFOF(formula))
      case TFF.Sequent(_, _) => throw new IllegalArgumentException(s"Unsupported expression in downgrade: ${statement.pretty}")
    }
  }

  final def tffLogicFormulaToFOF(formula: TPTP.TFF.Formula): TPTP.FOF.Formula = {
    import TPTP.FOF
    import TPTP.TFF
    formula match {
      case TFF.AtomicFormula(f, args) => FOF.AtomicFormula(f, args.map(tffTermToFOF))
      case TFF.QuantifiedFormula(quantifier, variableList, body) =>
        if (variableList.flatMap(_._2).forall(ty => ty == TPTP.TFF.AtomicType("$i", Seq.empty))) {
          quantifier match {
            case TFF.! => FOF.QuantifiedFormula(FOF.!, variableList.map(_._1), tffLogicFormulaToFOF(body))
            case TFF.? => FOF.QuantifiedFormula(FOF.?, variableList.map(_._1), tffLogicFormulaToFOF(body))
            case TFF.Epsilon => FOF.QuantifiedFormula(FOF.Epsilon, variableList.map(_._1), tffLogicFormulaToFOF(body))
            case _ => throw new IllegalArgumentException(s"Cannot downgrade TFF quantification to FOF, unsupported quantifier: ${quantifier.pretty}")
          }
        } else {
          throw new IllegalArgumentException(s"Cannot downgrade TFF quantification to FOF with other types than $$i: ${formula.pretty}")
        }
      case TFF.UnaryFormula(connective, body) =>
        connective match {
          case TFF.~ => FOF.UnaryFormula(FOF.~, tffLogicFormulaToFOF(body))
        }
      case TFF.BinaryFormula(connective, left, right) =>
        def connectiveAsFOF(connective: TFF.BinaryConnective): FOF.BinaryConnective = connective match {
          case TFF.<=> => FOF.<=>
          case TFF.Impl => FOF.Impl
          case TFF.<= => FOF.<=
          case TFF.<~> => FOF.<~>
          case TFF.~| => FOF.~|
          case TFF.~& => FOF.~&
          case TFF.| =>  FOF.|
          case TFF.& => FOF.&
        }
        FOF.BinaryFormula(connectiveAsFOF(connective), tffLogicFormulaToFOF(left), tffLogicFormulaToFOF(right))
      case TFF.Equality(left, right) => FOF.Equality(tffTermToFOF(left), tffTermToFOF(right))
      case TFF.Inequality(left, right) => FOF.Inequality(tffTermToFOF(left), tffTermToFOF(right))

      case _ => throw new IllegalArgumentException(s"Cannot downgrade TFF expression to FOF, outside of language fragment: ${formula.pretty}")
    }
  }
  final def tffTermToFOF(term: TPTP.TFF.Term): TPTP.FOF.Term = {
    import TPTP.TFF
    import TPTP.FOF
    term match {
      case TFF.AtomicTerm(f, args) => FOF.AtomicTerm(f, args.map(tffTermToFOF))
      case TFF.Variable(name) => FOF.Variable(name)
      case TFF.DistinctObject(name) => FOF.DistinctObject(name)
      case TFF.NumberTerm(value) => FOF.NumberTerm(value)
      case _ => throw new IllegalArgumentException(s"Cannot downgrade TFF expression to FOF, outside of language fragment: ${term.pretty}")
    }
  }

}
