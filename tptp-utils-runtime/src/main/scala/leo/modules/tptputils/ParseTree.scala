package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.TFF

object ParseTree {

  final def toJSON(problem: TPTP.Problem): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("[\n")
    problem.includes foreach { inc => sb.append("  "); sb.append(include(inc)); sb.append("\n")}
    problem.formulas foreach { f => sb.append("  "); sb.append(annotatedFormula(f)); sb.append("\n")}
    sb.append("]")
    sb.toString()
  }

  private[this] final def include(include: TPTP.Include): String = {
    if (include._2.isEmpty) s"{ type : 'include', file : '${include._1}' }"
    else s"{ type : 'include', file : '${include._1}', selection : ${include._2.mkString("[", ",", "]")} }"
  }

  private[this] final def annotatedFormula(annotatedFormula: TPTP.AnnotatedFormula): String = {
    val prefix = s"{ type : 'annotatedFormula', language : '${annotatedFormula.formulaType.toString}', name : '${annotatedFormula.name}', role : '${annotatedFormula.role}', annotations : '${annotatedFormula.annotations.toString}'"
    val formulaJSON: String = annotatedFormula match {
      case TPTP.THFAnnotated(_, _, formula, _) => thfStatement(formula)
      case TPTP.TFFAnnotated(_, _, formula, _) => tffStatement(formula)
      case TPTP.FOFAnnotated(_, _, formula, _) => fofStatement(formula)
      case TPTP.TCFAnnotated(_, _, formula, _) => tcfStatement(formula)
      case TPTP.CNFAnnotated(_, _, formula, _) => cnfStatement(formula)
      case TPTP.TPIAnnotated(_, _, formula, _) => fofStatement(formula)
    }
    s"$prefix, formula : $formulaJSON }"
  }

  private[this] final def thfStatement(thfStatement: TPTP.THF.Statement): String = ""

  private[this] final def tffStatement(tffStatement: TPTP.TFF.Statement): String = {
    tffStatement match {
      case TFF.Typing(atom, typ) => s"{ type : 'typing', name : '${atom}', body : ${tffType(typ)} }"
      case TFF.Logical(formula) => tffFormula(formula)
    }
  }
  private[this] final def tffFormula(formula: TPTP.TFF.Formula): String = formula match {
    case TFF.AtomicFormula(f, args) => s"{ type : 'atomicFormula' , name : '$f' , body : ${args.map(tffTerm).mkString("[", ", ", "]")} }"
    case TFF.QuantifiedFormula(quantifier, variableList, body) => s"{ type : 'quantifier' , quantifier : '${quantifier.pretty}' , vars : '' , body : [${tffFormula(body)}] }"
    case TFF.UnaryFormula(connective, body) => s"{ type : 'connective' , connective : '${connective.pretty}' , body : [${tffFormula(body)}] }"
    case TFF.BinaryFormula(connective, left, right) => s"{ type : 'connective' , connective : '${connective.pretty}' , body : [${tffFormula(left)}, ${tffFormula(right)}] }"
    case TFF.Equality(left, right) => s"{ type : 'connective' , connective : '=' , body : [${tffTerm(left)}, ${tffTerm(right)}] }"
    case TFF.Inequality(left, right) => s"{ type : 'connective' , connective : '!=' , body : [${tffTerm(left)}, ${tffTerm(right)}}] }"
    case TFF.FormulaVariable(name) => s"{ type : 'variable' , name : '$name' }"
    case TFF.ConditionalFormula(condition, thn, els) => s"{ type : 'connective' , connective : 'ite' , body : [${tffFormula(condition)}, ${tffTerm(thn)}, ${tffTerm(els)}}] }"
    case TFF.LetFormula(typing, binding, body) => ???
    case TFF.Assignment(lhs, rhs) => s"{ type : 'connective' , connective : 'assignment' , body : [${tffTerm(lhs)}, ${tffTerm(rhs)}] }"
    case TFF.MetaIdentity(lhs, rhs) => s"{ type : 'connective' , connective : 'metaEq' , body : [${tffTerm(lhs)}, ${tffTerm(rhs)}] }"
    case TFF.NonclassicalPolyaryFormula(connective, args) => s"{ type : 'connective' , connective : '' , body : ${args.map(tffFormula).mkString("[", ", ", "]")} }"
  }
  private[this] final def tffTerm(term: TPTP.TFF.Term): String = term match {
    case TFF.AtomicTerm(f, args) => s"{ type : 'atomicTerm' , name : '$f' , body : ${args.map(tffTerm).mkString("[", ", ", "]")} }"
    case TFF.Variable(name) => s"{ type : 'variable' , name : '$name' }"
    case TFF.DistinctObject(name) => s"{ type : 'distinct' , name : '$name' }"
    case TFF.NumberTerm(value) => s"{ type : 'number' , value : '${value.pretty}' }"
    case TFF.Tuple(elements) => s"{ type : 'tuple' , body : ${elements.map(tffTerm).mkString("[", ", ", "]")} }"
    case TFF.FormulaTerm(formula) => s"{ type : 'formulaTerm' , body : [${tffFormula(formula)}] }"
  }
  private[this] final def tffType(typ: TPTP.TFF.Type): String = typ match {
    case TFF.AtomicType(name, args) => s"{ type : 'atomicType' , name : '$name' , body : ${args.map(tffType).mkString("[", ", ", "]")} }"
    case TFF.MappingType(left, right) => s"{ type : 'connective' , connective : '>' , body : [${left.map(tffType).mkString(",")}, ${tffType(right)}] }"
    case TFF.QuantifiedType(variables, body) => s"{ type : 'quantifier' , quantifier : '!' , vars : ${variables.map(_._1).mkString("[", ", ", "]")} , body : [${tffType(body)}] }"
    case TFF.TypeVariable(name) => s"{ type : 'variable' , name : '$name' }"
    case TFF.TupleType(components) => s"{ type : 'tuple' , body : ${components.map(tffType).mkString("[", ", ", "]")} }"
  }

  private[this] final def fofStatement(thfStatement: TPTP.FOF.Statement): String = ""
  private[this] final def tcfStatement(thfStatement: TPTP.TCF.Statement): String = ""
  private[this] final def cnfStatement(thfStatement: TPTP.CNF.Statement): String = ""
}