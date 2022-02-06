package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.CNF

object SyntaxTransform {
  @inline final def tffToTHF(tff: TPTP.TFFAnnotated): TPTP.THFAnnotated =
    TPTP.THFAnnotated(tff.name, tff.role, tffStatementToTHF(tff.formula), tff.annotations)
  @inline final def tffToTHF(tffs: Seq[TPTP.TFFAnnotated]): Seq[TPTP.THFAnnotated] = tffs.map(tffToTHF)
  @inline final def fofToTHF(fof: TPTP.FOFAnnotated): TPTP.THFAnnotated = tffToTHF(fofToTFF(fof))
  @inline final def fofToTHF(fofs: Seq[TPTP.FOFAnnotated]): Seq[TPTP.THFAnnotated] = fofs.map(fofToTHF)
  @inline final def tcfToTHF(tcf: TPTP.TCFAnnotated): TPTP.THFAnnotated = tffToTHF(tcfToTFF(tcf))
  @inline final def tcfToTHF(tcfs: Seq[TPTP.TCFAnnotated]): Seq[TPTP.THFAnnotated] = tcfs.map(tcfToTHF)
  @inline final def cnfToTHF(cnf: TPTP.CNFAnnotated): TPTP.THFAnnotated = fofToTHF(cnfToFOF(cnf))
  @inline final def cnfToTHF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.THFAnnotated] = cnfs.map(cnfToTHF)

  @inline final def fofToTFF(fof: TPTP.FOFAnnotated): TPTP.TFFAnnotated =
    TPTP.TFFAnnotated(fof.name, fof.role, fofStatementToTFF(fof.formula), fof.annotations)
  @inline final def fofToTFF(fofs: Seq[TPTP.FOFAnnotated]): Seq[TPTP.TFFAnnotated] = fofs.map(fofToTFF)
  @inline final def tcfToTFF(tcf: TPTP.TCFAnnotated): TPTP.TFFAnnotated =
    TPTP.TFFAnnotated(tcf.name, tcf.role, tcfStatementToTFF(tcf.formula), tcf.annotations)
  @inline final def tcfToTFF(tcfs: Seq[TPTP.TCFAnnotated]): Seq[TPTP.TFFAnnotated] = tcfs.map(tcfToTFF)
  @inline final def cnfToTFF(cnf: TPTP.CNFAnnotated): TPTP.TFFAnnotated = fofToTFF(cnfToFOF(cnf))
  @inline final def cnfToTFF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.TFFAnnotated] = cnfs.map(cnfToTFF)

  @inline final def cnfToFOF(cnf: TPTP.CNFAnnotated): TPTP.FOFAnnotated =
    TPTP.FOFAnnotated(cnf.name, cnf.role, cnfStatementToFOF(cnf.formula), cnf.annotations)
  @inline final def cnfToFOF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.FOFAnnotated] = cnfs.map(cnfToFOF)

  @inline final def cnfToTCF(cnf: TPTP.CNFAnnotated): TPTP.TCFAnnotated = {
    cnf.formula match {
      case CNF.Logical(f) =>
        TPTP.TCFAnnotated(cnf.name, cnf.role, TPTP.TCF.Logical(TPTP.TCF.Formula(Seq.empty, f)), cnf.annotations)
    }
  }

  @inline final def cnfToTCF(cnfs: Seq[TPTP.CNFAnnotated]): Seq[TPTP.TCFAnnotated] = cnfs.map(cnfToTCF)

  final def transformAnnotatedFormula(goalLanguage: TPTP.AnnotatedFormula.FormulaType.FormulaType,
                                      annotatedFormula: TPTP.AnnotatedFormula): TPTP.AnnotatedFormula = {
    if (isMoreGeneralThan(goalLanguage, annotatedFormula.formulaType)) {
      (annotatedFormula: @unchecked) match {
        case f@TPTP.THFAnnotated(_, _, _, _) => f // no language is more general
        case f@TPTP.TFFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.THF => tffToTHF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => f
          }
        case f@TPTP.FOFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.THF => fofToTHF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => fofToTFF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => f
          }
        case f@TPTP.TCFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
          case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.THF => tcfToTHF(f)
          case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => tcfToTFF(f)
          case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => f
        }
        case f@TPTP.CNFAnnotated(_, _, _, _) =>
          (goalLanguage: @unchecked) match {
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.THF => cnfToTHF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TFF => cnfToTFF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.FOF => cnfToFOF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.TCF => cnfToTCF(f)
            case leo.datastructures.TPTP.AnnotatedFormula.FormulaType.CNF => f
          }
      }
    } else {
      throw new TPTPTransformException(s"Cannot transform ${annotatedFormula.formulaType.toString} formula '${annotatedFormula.name}' " +
        s"to a ${goalLanguage.toString} formula because the goal language is less expressive.")
    }
  }

  final def transformProblem(goalLanguage: TPTP.AnnotatedFormula.FormulaType.FormulaType,
                             problem: TPTP.Problem,
                             addMissingTypeDeclarations: Boolean = true): TPTP.Problem = {
    var transformedFormulas: Seq[TPTP.AnnotatedFormula] = Vector.empty
    for (formula <- problem.formulas) {
      transformedFormulas = transformedFormulas :+ transformAnnotatedFormula(goalLanguage, formula)
    }
    if (goalLanguage == TPTP.AnnotatedFormula.FormulaType.THF && addMissingTypeDeclarations) {
      val specs = generateMissingTypeDeclarations(problem.formulas)
      TPTP.Problem(problem.includes, specs ++ transformedFormulas, problem.formulaComments)
    } else TPTP.Problem(problem.includes, transformedFormulas, problem.formulaComments)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TFF TO THF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def tffStatementToTHF(statement: TPTP.TFF.Statement): TPTP.THF.Statement = {
    import TPTP.{TFF, THF}
    statement match {
      case TFF.Typing(atom, typ) => THF.Typing(atom, tffTypeToTHF(typ))
      case TFF.Logical(formula) => THF.Logical(tffLogicFormulaToTHF(formula))
    }
  }
  final def tffLogicFormulaToTHF(formula: TPTP.TFF.Formula): TPTP.THF.Formula = {
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
      case TFF.MetaIdentity(lhs, rhs) => THF.BinaryFormula(THF.==, tffTermToTHF(lhs), tffTermToTHF(rhs))
      case TFF.NonclassicalPolyaryFormula(connective, args) =>
        val connective0: THF.VararyConnective = connective match {
          case TFF.NonclassicalLongOperator(name, parameters) =>
            val parameters0 = parameters.map {
              case Left(index) => Left(tffTermToTHF(index))
              case Right((lhs, rhs)) => Right((tffTermToTHF(lhs), tffTermToTHF(rhs)))
            }
            THF.NonclassicalLongOperator(name, parameters0)
          case TFF.NonclassicalBox(index) => THF.NonclassicalBox(index.map(tffTermToTHF))
          case TFF.NonclassicalDiamond(index) => THF.NonclassicalDiamond(index.map(tffTermToTHF))
          case TFF.NonclassicalCone(index) => THF.NonclassicalCone(index.map(tffTermToTHF))
        }
        args.foldLeft[THF.Formula](THF.ConnectiveTerm(connective0)) { case (expr, arg) => THF.BinaryFormula(THF.App, expr, tffLogicFormulaToTHF(arg)) }
    }
  }
  final def tffUnaryConnectiveToTHF(conn: TPTP.TFF.UnaryConnective): TPTP.THF.UnaryConnective = {
    import TPTP.{TFF, THF}
    conn match {
      case TFF.~ => THF.~
    }
  }
  final def tffBinaryConnectiveToTHF(conn: TPTP.TFF.BinaryConnective): TPTP.THF.BinaryConnective = {
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

  final def tffTermToTHF(term: TPTP.TFF.Term): TPTP.THF.Formula = {
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

  final def tffTypeToTHF(typ: TPTP.TFF.Type): TPTP.THF.Type = {
    import TPTP.{TFF, THF}
    typ match {
      case TFF.AtomicType(name, args) =>
        args.foldLeft[THF.Type](THF.FunctionTerm(name, Seq.empty)) { case (expr, arg) => THF.BinaryFormula(THF.App, expr, tffTypeToTHF(arg)) }
      case TFF.MappingType(left, right) =>
        left.foldRight[THF.Formula](tffTypeToTHF(right)) { case (arg, expr) => THF.BinaryFormula(THF.FunTyConstructor, tffTypeToTHF(arg), expr) }
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
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // FOF TO TFF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def fofStatementToTFF(statement: TPTP.FOF.Statement): TPTP.TFF.Statement = {
    import TPTP.{FOF, TFF}
    statement match {
      case FOF.Logical(formula) => TFF.Logical(fofLogicFormulaToTFF(formula))
    }
  }

  final def fofLogicFormulaToTFF(formula: TPTP.FOF.Formula): TPTP.TFF.Formula = {
    import TPTP.{FOF, TFF}
    formula match {
      case FOF.AtomicFormula(f, args) => TFF.AtomicFormula(f, args.map(fofTermToTFF))
      case FOF.QuantifiedFormula(quantifier, variableList, body) =>
        val quantifier0 = quantifier match {
          case FOF.! => TFF.!
          case FOF.? => TFF.?
        }
        TFF.QuantifiedFormula(quantifier0, variableList.map(x => (x, None)), fofLogicFormulaToTFF(body))
      case FOF.UnaryFormula(connective, body) =>
        val connective0 = connective match {
          case FOF.~ => TFF.~
        }
        TFF.UnaryFormula(connective0, fofLogicFormulaToTFF(body))
      case FOF.BinaryFormula(connective, left, right) =>
        val connective0 = connective match {
          case FOF.<=> => TFF.<=>
          case FOF.Impl => TFF.Impl
          case FOF.<= => TFF.<=
          case FOF.<~> => TFF.<~>
          case FOF.~| => TFF.~|
          case FOF.~& => TFF.~&
          case FOF.| => TFF.|
          case FOF.& => TFF.&
        }
        TFF.BinaryFormula(connective0, fofLogicFormulaToTFF(left), fofLogicFormulaToTFF(right))
      case FOF.Equality(left, right) => TFF.Equality(fofTermToTFF(left), fofTermToTFF(right))
      case FOF.Inequality(left, right) => TFF.Inequality(fofTermToTFF(left), fofTermToTFF(right))
    }
  }

  final def fofTermToTFF(term: TPTP.FOF.Term): TPTP.TFF.Term = {
    import TPTP.{FOF, TFF}
    term match {
      case FOF.AtomicTerm(f, args) => TFF.AtomicTerm(f, args.map(fofTermToTFF))
      case FOF.Variable(name) => TFF.Variable(name)
      case FOF.DistinctObject(name) => TFF.DistinctObject(name)
      case FOF.NumberTerm(value) => TFF.NumberTerm(value)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // FOF TO TFF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def tcfStatementToTFF(statement: TPTP.TCF.Statement): TPTP.TFF.Statement = {
    import TPTP.{TCF, TFF}
    statement match {
      case TCF.Typing(atom, typ) => TFF.Typing(atom, typ)
      case TCF.Logical(formula) => TFF.Logical(tcfLogicFormulaToTFF(formula))
    }
  }

  final def tcfLogicFormulaToTFF(formula: TPTP.TCF.Formula): TPTP.TFF.Formula = {
    import TPTP.TFF
    formula.clause match {
      case Seq() => TPTP.TFF.AtomicFormula("$false", Seq.empty)
      case _ =>
        val (transformedLiterals, freeVars) = mapAndAccumulate(formula.clause, cnfLiteralToFOF)
        val transformedLiterals0 = transformedLiterals.map(fofLogicFormulaToTFF)
        val intermediate = transformedLiterals0.reduceRight(TFF.BinaryFormula(TFF.|, _, _))
        if (freeVars.isEmpty) intermediate
        else {
          val freeVars0: Seq[TFF.TypedVariable] = (freeVars diff formula.variables.map(_._1).toSet).map(x => (x, None)).toSeq
          TFF.QuantifiedFormula(TFF.!, formula.variables ++ freeVars0, intermediate)
        }
    }
  }
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // CNF TO FOF
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  final def cnfStatementToFOF(statement: TPTP.CNF.Statement): TPTP.FOF.Statement = {
    import TPTP.{CNF, FOF}
    statement match {
      case CNF.Logical(formula) => FOF.Logical(cnfLogicFormulaToFOF(formula))
    }
  }

  type CNFFreeVars = Set[String]
  final def cnfLogicFormulaToFOF(formula: TPTP.CNF.Formula): TPTP.FOF.Formula = {
    import TPTP.FOF
    formula match {
      case Seq() => FOF.AtomicFormula("$false", Seq.empty) // Should never happen, but just to be on the safe side
      case _ =>
        val (transformedLiterals, freeVars) = mapAndAccumulate(formula, cnfLiteralToFOF)
        val intermediate = transformedLiterals.reduceRight(FOF.BinaryFormula(FOF.|, _, _))
        if (freeVars.isEmpty) intermediate
        else FOF.QuantifiedFormula(FOF.!, freeVars.toSeq, intermediate)
    }
  }

  final def cnfLiteralToFOF(literal: TPTP.CNF.Literal): (TPTP.FOF.Formula, CNFFreeVars) = {
    import TPTP.{CNF, FOF}
    literal match {
      case CNF.PositiveAtomic(CNF.AtomicFormula(f, args)) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (FOF.AtomicFormula(f, translatedArgs), freeVars)
      case CNF.NegativeAtomic(CNF.AtomicFormula(f, args)) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (FOF.UnaryFormula(FOF.~, FOF.AtomicFormula(f, translatedArgs)), freeVars)
      case CNF.Equality(left, right) =>
        val (translatedLeft, fvsLeft) = cnfTermToFOF(left)
        val (translatedRight, fvsRight) = cnfTermToFOF(right)
        (FOF.Equality(translatedLeft, translatedRight), fvsLeft union fvsRight)
      case CNF.Inequality(left, right) =>
        val (translatedLeft, fvsLeft) = cnfTermToFOF(left)
        val (translatedRight, fvsRight) = cnfTermToFOF(right)
        (FOF.Inequality(translatedLeft, translatedRight), fvsLeft union fvsRight)
    }
  }

  final def cnfTermToFOF(term: TPTP.CNF.Term): (TPTP.FOF.Term, CNFFreeVars) = {
    import TPTP.{CNF, FOF}
    term match {
      case CNF.AtomicTerm(f, args) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (FOF.AtomicTerm(f, translatedArgs), freeVars)
      case CNF.Variable(name) => (FOF.Variable(name), Set(name))
      case CNF.DistinctObject(name) => (FOF.DistinctObject(name), Set.empty)
    }
  }

  private[this] final def mapAndAccumulate[A,B,C](list: Seq[A], f: A => (B, Set[C])): (Seq[B], Set[C]) = {
    var mapResult: Seq[B] = Seq.empty
    var accResult: Set[C] = Set.empty
    list foreach { x =>
      val fResult = f(x)
      mapResult = mapResult :+ fResult._1
      accResult = accResult union fResult._2
    }
    (mapResult, accResult)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Generate missing type declarations
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  private[this] def generateMissingTypeDeclarations(originalFormulas: Seq[TPTP.AnnotatedFormula]): Seq[TPTP.AnnotatedFormula] = {
    import scala.collection.mutable
    val symbolsWithType: mutable.Set[String] = mutable.Set.empty
    val symbolsMap: mutable.Map[String, TPTP.AnnotatedFormula] = mutable.Map.empty
    originalFormulas foreach {
      case TPTP.THFAnnotated(_, "type", TPTP.THF.Typing(s, _), _) => symbolsWithType += s
      case TPTP.TFFAnnotated(_, "type", TPTP.TFF.Typing(s, _), _) => symbolsWithType += s
      case f =>
        if (!Seq(TPTP.AnnotatedFormula.FormulaType.THF, TPTP.AnnotatedFormula.FormulaType.TPI).contains(f.formulaType)) {
          val symbolsInFormula = f.symbols.filterNot(s => s.startsWith("\"") || s.startsWith("$")) //exclude distinct objects and defined symbols
          for (s <- symbolsInFormula) {
            if (!symbolsMap.isDefinedAt(s)) symbolsMap += (s -> f)
          }
        }
    }
    val unspecifiedSymbols = (symbolsMap.keySet diff symbolsWithType).toSeq
//    val freshTypeSpecifications: mutable.Map[String, TPTP.THFAnnotated] = mutable.Map.empty TODO: For what was this intended?
    unspecifiedSymbols.map { s =>
      val ty = getTypeFromSymbolOccurence(s, symbolsMap(s))
      val spec = TPTP.THF.Typing(s, ty)
      TPTP.THFAnnotated(s"${s}_type", "type", spec, None)
    }
  }

  private[this] def getTypeFromSymbolOccurence(symbol: String, formula: TPTP.AnnotatedFormula): TPTP.THF.Type = {
    val result = formula match {
      case TPTP.TFFAnnotated(_, _, TPTP.TFF.Logical(formula), _) => getTypeFromTFFFormula(symbol, formula)
      case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(formula), _) => getTypeFromFOFFormula(symbol, formula)
      case TPTP.TCFAnnotated(_, _, TPTP.TCF.Logical(formula), _) => getTypeFromTCFFormula(symbol, formula)
      case TPTP.CNFAnnotated(_, _, TPTP.CNF.Logical(formula), _) => getTypeFromCNFFormula(symbol, formula)
      case _ => throw new TPTPTransformException("unexpected")
    }
    if (result.isDefined) result.get
    else throw new TPTPTransformException("unexpected")
  }

  private[this] def getTypeFromTFFFormula(symbol: String, formula: TPTP.TFF.Formula): Option[TPTP.THF.Type] = {
    import TPTP.TFF
    formula match {
      case TFF.AtomicFormula(f, args) =>
        if (f == symbol) Some(simplePredType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromTFFTerm(symbol, argsIt.next())
          }
          result
        }
      case TFF.QuantifiedFormula(_, _, body) => getTypeFromTFFFormula(symbol, body)
      case TFF.UnaryFormula(_, body) => getTypeFromTFFFormula(symbol, body)
      case TFF.BinaryFormula(_, left, right) =>
        val leftResult = getTypeFromTFFFormula(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromTFFFormula(symbol, right)
      case TFF.Equality(left, right) =>
        val leftResult = getTypeFromTFFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromTFFTerm(symbol, right)
      case TFF.Inequality(left, right) =>
        val leftResult = getTypeFromTFFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromTFFTerm(symbol, right)
      case TFF.FormulaVariable(_) => None
      case TFF.ConditionalFormula(condition, thn, els) =>
        val condResult = getTypeFromTFFFormula(symbol, condition)
        if (condResult.isDefined) condResult
        else {
          val thnResult = getTypeFromTFFTerm(symbol, thn)
          if (thnResult.isDefined) thnResult
          else getTypeFromTFFTerm(symbol, els)
        }
      case TFF.LetFormula(_, _, body) => getTypeFromTFFTerm(symbol, body)
      case TFF.Assignment(_, _) => None
      case TFF.MetaIdentity(_, _) => None
      case TFF.NonclassicalPolyaryFormula(_, args) =>
        val argsIt = args.iterator
        var result: Option[TPTP.THF.Type] = None
        while (argsIt.hasNext && result.isEmpty) {
          result = getTypeFromTFFFormula(symbol, argsIt.next())
        }
        result
    }
  }

  private[this] def getTypeFromTFFTerm(symbol: String, term: TPTP.TFF.Term): Option[TPTP.THF.Type] = {
    import TPTP.TFF
    term match {
      case TFF.AtomicTerm(f, args) =>
        if (f == symbol) Some(simpleFunType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromTFFTerm(symbol, argsIt.next())
          }
          result
        }
      case TFF.Variable(_) => None
      case TFF.DistinctObject(_) => None
      case TFF.NumberTerm(_) => None
      case TFF.Tuple(elements) =>
        val elementsIt = elements.iterator
        var result: Option[TPTP.THF.Type] = None
        while (elementsIt.hasNext && result.isEmpty) {
          result = getTypeFromTFFTerm(symbol, elementsIt.next())
        }
        result
      case TFF.FormulaTerm(formula) => getTypeFromTFFFormula(symbol, formula)
    }
  }

  private[this] def getTypeFromFOFFormula(symbol: String, formula: TPTP.FOF.Formula): Option[TPTP.THF.Type] = {
    import TPTP.FOF
    formula match {
      case FOF.AtomicFormula(f, args) =>
        if (f == symbol) Some(simplePredType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromFOFTerm(symbol, argsIt.next())
          }
          result
        }
      case FOF.QuantifiedFormula(_, _, body) => getTypeFromFOFFormula(symbol, body)
      case FOF.UnaryFormula(_, body) => getTypeFromFOFFormula(symbol, body)
      case FOF.BinaryFormula(_, left, right) =>
        val leftResult = getTypeFromFOFFormula(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromFOFFormula(symbol, right)
      case FOF.Equality(left, right) =>
        val leftResult = getTypeFromFOFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromFOFTerm(symbol, right)
      case FOF.Inequality(left, right) =>
        val leftResult = getTypeFromFOFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromFOFTerm(symbol, right)
    }
  }
  private[this] def getTypeFromFOFTerm(symbol: String, term: TPTP.FOF.Term): Option[TPTP.THF.Type] = {
    import TPTP.FOF
    term match {
      case FOF.AtomicTerm(f, args) =>
        if (f == symbol) Some(simpleFunType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromFOFTerm(symbol, argsIt.next())
          }
          result
        }
      case FOF.Variable(_) => None
      case FOF.DistinctObject(_) => None
      case FOF.NumberTerm(_) => None
    }
  }

  private[this] def getTypeFromTCFFormula(symbol: String, formula: TPTP.TCF.Formula): Option[TPTP.THF.Type] = {
    getTypeFromCNFFormula(symbol, formula.clause)
  }

  private[this] def getTypeFromCNFFormula(symbol: String, formula: TPTP.CNF.Formula): Option[TPTP.THF.Type] = {
    val litsIt = formula.iterator
    var result: Option[TPTP.THF.Type] = None
    while (litsIt.hasNext && result.isEmpty) {
      result = getTypeFromCNFLiteral(symbol, litsIt.next())
    }
    result
  }
  private[this] def getTypeFromCNFLiteral(symbol: String, literal: TPTP.CNF.Literal): Option[TPTP.THF.Type] = {
    import TPTP.CNF
    literal match {
      case CNF.PositiveAtomic(CNF.AtomicFormula(f, args)) =>
        if (f == symbol) Some(simplePredType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromCNFTerm(symbol, argsIt.next())
          }
          result
        }
      case CNF.NegativeAtomic(CNF.AtomicFormula(f, args)) =>
        if (f == symbol) Some(simplePredType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromCNFTerm(symbol, argsIt.next())
          }
          result
        }
      case CNF.Equality(left, right) =>
        val leftResult = getTypeFromCNFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromCNFTerm(symbol, right)
      case CNF.Inequality(left, right) =>
        val leftResult = getTypeFromCNFTerm(symbol, left)
        if (leftResult.isDefined) leftResult
        else getTypeFromCNFTerm(symbol, right)
    }
  }
  private[this] def getTypeFromCNFTerm(symbol: String, term: TPTP.CNF.Term): Option[TPTP.THF.Type] = {
    import TPTP.CNF
    term match {
      case CNF.AtomicTerm(f, args) =>
        if (f == symbol) Some(simpleFunType(args.size))
        else {
          val argsIt = args.iterator
          var result: Option[TPTP.THF.Type] = None
          while (argsIt.hasNext && result.isEmpty) {
            result = getTypeFromCNFTerm(symbol, argsIt.next())
          }
          result
        }
      case CNF.Variable(_) => None
      case CNF.DistinctObject(_) => None
    }
  }

  private[this] def simplePredType(n: Int): TPTP.THF.Type = {
    val typesAsList: Seq[TPTP.THF.Type] = Seq.fill(n)(TPTP.THF.FunctionTerm("$i", Seq.empty)) :+ TPTP.THF.FunctionTerm("$o", Seq.empty)
    typesAsList.reduceRight { TPTP.THF.BinaryFormula(TPTP.THF.FunTyConstructor, _, _) }
  }
  private[this] def simpleFunType(n: Int): TPTP.THF.Type = {
    val typesAsList: Seq[TPTP.THF.Type] = Seq.fill(n)(TPTP.THF.FunctionTerm("$i", Seq.empty)) :+ TPTP.THF.FunctionTerm("$i", Seq.empty)
    typesAsList.reduceRight { TPTP.THF.BinaryFormula(TPTP.THF.FunTyConstructor, _, _) }
  }
  /////////////////// Generate type declarations END /////////////////////////////
}
