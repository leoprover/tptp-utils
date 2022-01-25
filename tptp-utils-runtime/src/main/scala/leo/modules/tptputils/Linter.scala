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
  private[this] final def lintLogicSpecTHFFormula(formula: THF.Statement): Seq[String] = formula match {
    case THF.Logical(THF.BinaryFormula(THF.==, ft@THF.FunctionTerm(logicName, Seq()), right)) =>
      if (ft.isDefinedFunction) {
        logicName match {
          case "$modal" => lintModalLogicConfig(right)
          case _ => Seq(info(s"Logic $logicName is unknown, cannot analyze its specification; skipping."))
        }
      } else if (ft.isSystemFunction) Seq(info(s"$logicName is a system-defined logic, cannot analyze its specification; skipping."))
      else Seq(error(s"Logic names on the LHS of a logic specification formulas must be defined names or system names, but $logicName is given."))
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }
  private[this] final def lintLogicSpecTFFFormula(formula: TFF.Statement): Seq[String] = formula match {
    case TFF.Logical(TFF.MetaIdentity(ft@TFF.AtomicTerm(logicName, Seq()), rhs)) =>
      if (ft.isDefinedFunction) {
        logicName match {
          case "$modal" =>
            import SyntaxTransform.tffTermToTHF
            lintModalLogicConfig(tffTermToTHF(rhs))
          case _ => Seq(info(s"Logic $logicName is unknown, cannot analyze its specification; skipping."))
        }
      } else if (ft.isSystemFunction) Seq(info(s"$logicName is a system-defined logic, cannot analyze its specification; skipping."))
      else Seq(error(s"Logic names on the LHS of a logic specification formulas must be defined names or system names, but $logicName is given."))
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }

  private[this] final def lintModalLogicConfig(formula: THF.Formula): Seq[String] = formula match {
    case THF.Tuple(elements) =>
      val buffer: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
      var modalitiesSpecified = false
      var quantificationSpecified = false
      var rigiditySpecified = false
      var consequenceSpecified = false
      elements.foreach {
        case THF.BinaryFormula(THF.==, left, right) => left match {
          case ft@THF.FunctionTerm(parameter, Seq()) if ft.isDefinedFunction => parameter match {
            case "$consequence" =>
              buffer.appendAll(lintModalLogicConfigStandardEntry("$consequence", Seq("$local", "$global"), right))
              consequenceSpecified = true
            case "$modalities" =>
              buffer.appendAll(lintModalLogicConfigModalitiesEntry(right))
              modalitiesSpecified = true
            case "$quantification" =>
              buffer.appendAll(lintModalLogicConfigStandardEntry("$quantification", Seq("$varying", "$constant", "$cumulative", "$decreasing"), right))
              quantificationSpecified = true
            case "$constants" =>
              buffer.appendAll(lintModalLogicConfigStandardEntry("$constants", Seq("$rigid", "$flexible"), right))
              rigiditySpecified = true
            case _ => buffer.append(error(s"Unknown parameter name $parameter for modal logic specification."))
          }
          case _ => buffer.append(error(s"Malformed logic specification parameter ${left.pretty}, only defined words are allowed."))
        }
        case x => buffer.append(error(s"Unknown logic specification entry for modal logic: ${x.pretty}"))
      }
      if (!modalitiesSpecified) buffer.append(error("Modal logic specification incomplete: $modalities entry missing."))
      if (!quantificationSpecified) buffer.append(error("Modal logic specification incomplete: $quantification entry missing."))
      if (!rigiditySpecified) buffer.append(error("Modal logic specification incomplete: $constants entry missing."))
      if (!consequenceSpecified) buffer.append(error("Modal logic specification incomplete: $consequence entry missing."))
      buffer.toSeq
    case _ => Seq(error(s"Malformed logic specification: ${formula.pretty}"))
  }
  @inline private[this] final def lintModalLogicConfigStandardEntry(name: String, allowedValues: Seq[String], right: THF.Formula): Seq[String] = {
    val buffer: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
    var defaultValue = false
    right match {
      case THF.FunctionTerm(arg, Seq()) => arg match {
        case _ if allowedValues.contains(arg) => ()
        case _ => buffer.append(error(s"Unknown argument $arg to parameter $name."))
      }
      case THF.Tuple(elements) => elements.foreach {
        case THF.FunctionTerm(arg, Seq()) => arg match {
          case _ if allowedValues.contains(arg) =>
            if (defaultValue) buffer.append(warning(s"Duplicated default value for $name. It's not clear what default value the ATP system will use; remove all except one."))
            else defaultValue = true
          case _ => buffer.append(error(s"Unknown argument $arg to parameter $name."))
        }
        case THF.BinaryFormula(THF.==, THF.FunctionTerm(identifier, Seq()), THF.FunctionTerm(arg, Seq())) => arg match {
          case _ if allowedValues.contains(arg) => ()
          case _ => buffer.append(error(s"Unknown argument $arg to parameter $name for item $identifier."))
        }
        case x => buffer.append(error(s"Unknown argument ${x.pretty} to parameter $name."))
      }
      case x => buffer.append(error(s"Unknown argument ${x.pretty} to parameter $name."))
    }
    buffer.toSeq
  }
  @inline private[this] final def lintModalLogicConfigModalitiesEntry(right: THF.Formula): Seq[String] = {
    val buffer: collection.mutable.Buffer[String] = collection.mutable.Buffer.empty
    val name = "$modalities"
    var defaultValue = false
    right match {
      case THF.FunctionTerm(arg, Seq()) => arg match {
        case _ if arg.startsWith("$modal_system_") => ()
        case _ => buffer.append(error(s"Unknown argument $arg to parameter $name."))
      }
      case THF.Tuple(elements) => elements.foreach {
        case THF.FunctionTerm(arg, Seq()) => arg match {
          case _ if arg.startsWith("$modal_system_") =>
            if (defaultValue) buffer.append(warning(s"Duplicated default value for $name. It's not clear what default value the ATP system will use; remove all except one."))
            else defaultValue = true
          case _ if arg.startsWith("$modal_axiom_") => ()
          case _ => buffer.append(error(s"Unknown argument $arg to parameter $name."))
        }
        case THF.BinaryFormula(THF.==, op@THF.ConnectiveTerm(conn), rhs) =>
          conn match {
            case THF.NonclassicalLongOperator(connName, params) if Seq("$box", "$diamond").contains(connName) =>
              params match {
                case Seq(Left(_)) => ()
                case Seq() => buffer.append(error("Assigning properties to non-indexed modal operator, use parameter default value for this."))
                case _ => buffer.append(error(s"Malformed arguments to modal operator ${op.pretty}, use parameter default value for this."))
              }
            case THF.NonclassicalBox(idx) => idx match {
              case Some(_) => ()
              case None => buffer.append(error("Assigning properties to non-indexed modal operator, use parameter default value for this."))
            }
            case THF.NonclassicalDiamond(idx) => idx match {
              case Some(_) => ()
              case None => buffer.append(error("Assigning properties to non-indexed modal operator, use parameter default value for this."))
            }
            case _ => buffer.append(error(s"Assigning properties '${rhs.pretty}' to classical operator ${conn.pretty}."))
          }
          rhs match {
            case THF.FunctionTerm(system, Seq()) if system.startsWith("$modal_system_") => ()
            case THF.Tuple(elements0) => elements0.foreach {
              case THF.FunctionTerm(axiom, Seq()) if axiom.startsWith("$modal_axiom_") => ()
              case x => buffer.append(error(s"Unknown or malformed argument ${x.pretty} to parameter $name for item ${op.pretty}."))
            }
            case _ => buffer.append(error(s"Unknown or malformed argument ${rhs.pretty} to parameter $name for item ${op.pretty}."))
          }
        case x => buffer.append(error(s"Unknown argument ${x.pretty} to parameter $name."))
      }
      case _ => buffer.append(error(s"Malformed argument ${right.pretty} to parameter $name."))
    }
    buffer.toSeq
  }
}
