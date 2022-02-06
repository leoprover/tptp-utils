package leo.modules

import leo.datastructures.TPTP.AnnotatedFormula.FormulaType

package object tptputils {

  class TPTPTransformException(message: String) extends RuntimeException(message)

  /** Returns true iff `role` does not contain a subrole (i.e., role followed by dash followed by general term). */
  final def isSimpleRole(role: String): Boolean = !role.contains("-")
  /** Returns true iff `role` does contain a subrole (i.e., role followed by dash followed by general term). */
  final def isComplexRole(role: String): Boolean = !isSimpleRole(role)
  /** Returns None iff `role` is a simple role; otherwise the subrole part of the role. */
  final def getSubrole(role: String): Option[String] = if (isComplexRole(role)) {
    Some(roleSplit(role)._2.tail)
  } else None
  /** Returns the non-subrole part of the role. If no subrole-part exists, the role itself it returned. */
  final def toSimpleRole(role: String): String = roleSplit(role)._1
  // roleSplit("<prefix>-<suffix>") gives ("<prefix>", "-<suffix>"), i.e., the dash is still in there.
  private[this] final def roleSplit(role: String): (String, String) = role.span(_ != '-')

  final def isMoreGeneralThan(a: FormulaType.FormulaType, b: FormulaType.FormulaType): Boolean = {
    import FormulaType._
    a match {
      case THF => b match {
        case TPI => false
        case _ => true
      }
      case TFF => b match {
        case THF | TPI => false
        case _ => true
      }
      case FOF => b match {
        case FOF | CNF => true
        case _ => false
      }
      case TCF => b match {
        case TCF | CNF => true
        case _ => false
      }
      case CNF => b match {
        case CNF => true
        case _ => false
      }
      case TPI => false
    }
  }

}
