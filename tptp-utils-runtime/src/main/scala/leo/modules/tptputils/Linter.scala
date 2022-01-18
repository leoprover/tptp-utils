package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.datastructures.TPTP.{AnnotatedFormula, Problem, TFF, THF}

object Linter {
  @inline final private[this] def error(error: String): String = s"ERROR: $error"
  @inline final private[this] def warning(warning: String): String = s"WARNING: $warning"
  @inline final private[this] def info(info: String): String = s"INFO: $info"

  final def apply(problem: Problem): Seq[String] = {
    val buffer: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
    val usedNames: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
    problem.formulas.foreach { annotatedFormula =>

      // Uniqueness of names
      if (buffer.contains(annotatedFormula.name)) buffer.append(warning(s"Duplicated name ${annotatedFormula.name} in annotated formula."))
      else usedNames.append(annotatedFormula.name)

      // Logic specification linting
      if (annotatedFormula.role == "logic") buffer.appendAll(lintLogicSpec(annotatedFormula))

      // TODO:
      // NCL connectives only used if logic spec is present
      // Only the connectives used for which the logic has been specified
      // If multiple logics then no short forms
      // All types declared
      // Unused symbols
      // ...
    }
    buffer.toSeq
  }

  final def lintLogicSpec(spec: AnnotatedFormula): Seq[String] = {
    spec match {
      case TPTP.THFAnnotated(_, _, formula, _) => lintLogicSpecTHFFormula(formula)
      case TPTP.TFFAnnotated(_, _, formula, _) => lintLogicSpecTFFFormula(formula)
      case TPTP.FOFAnnotated(_, _, _, _) =>
        Seq(error("Logic specifications are only supported for TFF and THF, but FOF was given."))
      case TPTP.TCFAnnotated(_, _, _, _) =>
        Seq(error("Logic specifications are only supported for TFF and THF, but TCF was given."))
      case TPTP.CNFAnnotated(_, _, _, _) =>
        Seq(error("Logic specifications are only supported for TFF and THF, but CNF was given."))
      case TPTP.TPIAnnotated(_, _, _, _) =>
        Seq(error("Logic specifications are only supported for TFF and THF, but TPI was given."))
    }
  }
  private[this] def lintLogicSpecTHFFormula(formula: THF.Statement): Seq[String] = formula match {
    case THF.Logical(THF.BinaryFormula(THF.==, ft@THF.FunctionTerm(logicName, Seq()), right)) =>
      if (ft.isDefinedFunction) {
        logicName match {
          case "$modal" => lintModalLogicConfig(right)
          case _ => Seq(info(s"Logic $logicName is unknown, cannot analyze its specification; skipping."))
        }
      } else if (ft.isSystemFunction) Seq(info(s"$logicName is a system-defined logic, cannot analyze its specification; skipping."))
      else Seq(error(s"Logic names on the LHS of a logic specification formulas must be defined names or system names, but ${logicName} is given."))
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }
  private[this] def lintLogicSpecTFFFormula(formula: TFF.Statement): Seq[String] = formula match {
    case TFF.Logical(TFF.MetaIdentity(ft@TFF.AtomicTerm(logicName, Seq()), rhs)) =>
      if (ft.isDefinedFunction) {
        logicName match {
          case "$modal" =>
            import SyntaxTransform.tffTermToTHF
            lintModalLogicConfig(tffTermToTHF(rhs))
          case _ => Seq(info(s"Logic $logicName is unknown, cannot analyze its specification; skipping."))
        }
      } else if (ft.isSystemFunction) Seq(info(s"$logicName is a system-defined logic, cannot analyze its specification; skipping."))
      else Seq(error(s"Logic names on the LHS of a logic specification formulas must be defined names or system names, but ${logicName} is given."))
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }

  private[this] def lintModalLogicConfig(formula: THF.Formula): Seq[String] = formula match {
    case THF.Tuple(elements) =>
      val buffer: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
      var modalitiesSpecified = false
      var quantificationSpecified = false
      var rigiditySpecified = false
      var consequenceSpecified = false
      elements.foreach { elem =>
        ???
      }
      if (!modalitiesSpecified) error("Modal logic specification incomplete: $modalities entry missing.")
      if (!quantificationSpecified) error("Modal logic specification incomplete: $quantification entry missing.")
      if (!rigiditySpecified) error("Modal logic specification incomplete: $rigidity entry missing.")
      if (!consequenceSpecified) error("Modal logic specification incomplete: $consequence entry missing.")
      buffer.toSeq
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }
}
