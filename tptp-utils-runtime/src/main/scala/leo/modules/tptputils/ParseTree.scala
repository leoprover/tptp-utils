package leo.modules.tptputils

import leo.datastructures.TPTP

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
  private[this] final def tffStatement(thfStatement: TPTP.TFF.Statement): String = ""
  private[this] final def fofStatement(thfStatement: TPTP.FOF.Statement): String = ""
  private[this] final def tcfStatement(thfStatement: TPTP.TCF.Statement): String = ""
  private[this] final def cnfStatement(thfStatement: TPTP.CNF.Statement): String = ""
}
