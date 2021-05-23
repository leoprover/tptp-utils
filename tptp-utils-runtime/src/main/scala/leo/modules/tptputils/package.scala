package leo.modules

import leo.datastructures.TPTP.AnnotatedFormula.FormulaType

package object tptputils {

  class TPTPTransformException(message: String) extends RuntimeException(message)

  final def isMoreGeneralThan(a: FormulaType.FormulaType, b: FormulaType.FormulaType): Boolean = {
    import FormulaType._
    a match {
      case THF => true
      case TFF => b match {
        case THF => false
        case _ => true
      }
      case FOF => b match {
        case THF | TFF | TCF => false
        case _ => true
      }
      case TCF => b match {
        case THF | TFF | FOF => false
        case _ => true
      }
      case CNF => b match {
        case CNF | TPI => true
        case _ => false
      }
      case TPI => false
    }
  }

}
