package leo.modules

import leo.datastructures.TPTP.AnnotatedFormula.FormulaType

package object tptputils {

  class TPTPTransformException(message: String) extends RuntimeException(message)

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
