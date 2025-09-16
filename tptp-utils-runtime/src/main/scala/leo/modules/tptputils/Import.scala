package leo.modules.tptputils

import leo.datastructures.TPTP

import java.nio.file.Path
import scala.io.Source

object Import {
  sealed abstract class ExternalLanguage
  case object LegalRuleML extends ExternalLanguage

  final def apply(file: Path, from: ExternalLanguage): TPTP.Problem = {
    from match {
      case LegalRuleML => new LegalRuleMLImport().apply(file)
    }
  }

  final class LegalRuleMLImport {
    private var counter = 1
    private def generateName(): String = { val name = s"'formula_$counter'"; counter += 1; name  }
    private def lrml(elem: xml.Node): Boolean = elem.prefix == "lrml"

    def apply(path: Path): TPTP.Problem = {
      try {
        val topNode = xml.XML.load(new SourceInputStream(Source.fromFile(path.toFile)))
        lrmlFile(topNode)
      } catch {
      case e: Throwable => throw new IllegalArgumentException(e.toString)
      }
    }

    def lrmlFile(node: xml.Elem): TPTP.Problem = {
        // Check if it's LegalRuleML
        if (node.label == "LegalRuleML" && node.prefix == "lrml") {
          // Then we process all the meta-data
          val references: collection.mutable.Map[String, String] = collection.mutable.Map.empty
          val legalReferenceBlocks = ((node \ "LegalReferences") ++ (node \ "References")).filter(lrml)
          legalReferenceBlocks.foreach { refBlock =>
            references.addAll(lrmlRefBlock(refBlock))
          }
//          println(references.toString())

          // Then we process associations
          val associationsFormulaToRef: collection.mutable.Map[String, Seq[String]] = collection.mutable.Map.empty
          val associationBlocks = (node \ "Associations").filter(lrml)
          associationBlocks.foreach { assocBlock =>
            associationsFormulaToRef.addAll(lrmlAssocBlock(assocBlock))
          }
//          println(associationsFormulaToRef.toString())
          val associations = associationsFormulaToRef.toMap

          // Then we process statements
          val statementBlocks = (node \ "Statements").filter(lrml)
//          println(statementBlocks.toString())
          val translatedStatements = statementBlocks.map { block =>
            lrmlStatements(block, associations)
          }

          TPTP.Problem(Seq.empty, translatedStatements.flatten, Map.empty)
        } else throw new IllegalArgumentException(s"Invalid or incorrect LegalRuleML file provided.")
    }

    private[this] def lrmlRefBlock(elem: xml.Node): Map[String, String] = {
      val refs = ((elem \ "LegalReference") ++ (elem \ "Reference")).filter(lrml)
      refs.flatMap { e =>
        val ref = e.attribute("refersTo")
        val id = e.attribute("refID")
        if (ref.isEmpty || id.isEmpty) None
        else Some(ref.get.head.text, id.get.head.text)
      }.toMap
    }

    private[this] def lrmlAssocBlock(elem: xml.Node): Map[String, Seq[String]] = {
      val assocs = (elem \ "Association").filter(lrml)
      val erg = assocs.flatMap { e =>
        val appliesSources0 = (e \ "appliesSource").filter(lrml)
        val appliesSources = appliesSources0.flatMap { as =>
          as.attribute("keyref") match {
            case Some(v) => if (v.startsWith("#")) Some(v.head.text.tail) else Some(v.head.text)
            case None => None
          }
        }
        val toTargets0 = (e \ "toTarget").filter(lrml)
        val resMap = toTargets0.flatMap { as =>
          as.attribute("keyref") match {
            case Some(v) =>
              val n = v.head.text
              val n0: String = if (n.startsWith("#")) n.tail else n
              Some((n0, appliesSources))
            case None => None
          }
        }
        resMap
      }
      erg.toMap
    }

    private[this] def lrmlStatements(elem: xml.Node, associations: Map[String, Seq[String]]): Seq[TPTP.AnnotatedFormula] = {
      val statements = elem \ "_"
      statements.map(lrmlStatement(_, associations))
    }
    private[this] def tptpProhibition(bearer: Option[TPTP.TFF.Term], left: TPTP.TFF.Formula, right: TPTP.TFF.Formula): TPTP.TFF.Formula = {
      bearer match {
        case Some(value) => TPTP.TFF.NonclassicalPolyaryFormula(
          TPTP.TFF.NonclassicalLongOperator("$$prohibition", None, Seq((TPTP.TFF.AtomicTerm("bearer", Seq.empty), value))),
          Seq(left,right))
        case None => TPTP.TFF.NonclassicalPolyaryFormula(
          TPTP.TFF.NonclassicalLongOperator("$$prohibition", None, Seq.empty),
          Seq(left,right))
      }
    }
    private[this] def tptpObligation(bearer: Option[TPTP.TFF.Term], left: TPTP.TFF.Formula, right: TPTP.TFF.Formula): TPTP.TFF.Formula = {
      bearer match {
        case Some(value) => TPTP.TFF.NonclassicalPolyaryFormula(
            TPTP.TFF.NonclassicalLongOperator("$$obligation", None, Seq((TPTP.TFF.AtomicTerm("bearer", Seq.empty), value))),
            Seq(left, right))
        case None => TPTP.TFF.NonclassicalPolyaryFormula(
          TPTP.TFF.NonclassicalLongOperator("$$obligation", None, Seq.empty),
          Seq(left, right))
      }
    }
    private[this] def tptpPermission(bearer: Option[TPTP.TFF.Term], left: TPTP.TFF.Formula, right: TPTP.TFF.Formula): TPTP.TFF.Formula = {
      bearer match {
        case Some(value) => TPTP.TFF.NonclassicalPolyaryFormula(
          TPTP.TFF.NonclassicalLongOperator("$$permission", None, Seq((TPTP.TFF.AtomicTerm("bearer", Seq.empty), value))),
          Seq(left, right))
        case None => TPTP.TFF.NonclassicalPolyaryFormula(
          TPTP.TFF.NonclassicalLongOperator("$$permission", None, Seq.empty),
          Seq(left, right))
      }
    }
    private[this] def tptpConstitutive(left: TPTP.TFF.Formula, right: TPTP.TFF.Formula): TPTP.TFF.Formula =
      TPTP.TFF.NonclassicalPolyaryFormula(TPTP.TFF.NonclassicalLongOperator("$$constitutive", None, Seq.empty), Seq(left,right))

    private[this] def lrmlStatement(elem: xml.Node, associations: Map[String, Seq[String]]): TPTP.AnnotatedFormula = {
      val formulaName = elem.attribute("key")
      val (name, annotations): (String, TPTP.Annotations) = formulaName match {
        case None => (generateName(), None)
        case Some(v) =>
          associations.get(v.head.text) match {
            case None => (v.head.text, None)
            case Some(y) =>
              val z = TPTP.GeneralTerm(Seq(TPTP.MetaFunctionData("sources", y.map(arg => TPTP.GeneralTerm(Seq(TPTP.MetaFunctionData(arg, Seq.empty)), None)))), None)
              (v.head.text, Some(z, None))
          }

      }

      elem.label match {
        case "ConstitutiveStatement" | "PrescriptiveStatement" =>
          val rule = elem \ "Rule"
          val convertedRule = if (rule.size != 1) throw new IllegalArgumentException("rule != 1")
          else {
            val rule0 = rule.head
            val left = (rule0 \ "if" \ "_")
            val right = (rule0 \ "then" \ "_")
            if (left.size == 1 && right.size == 1) {
              val left0 = left.head
              val right0 = right.head

              val body = lrmlFormula(left0)
              val (typ, bearer, head) = lrmlMaybeDeonticFormula(right0)

              typ match {
                case "Prohibition" if elem.label == "PrescriptiveStatement" => tptpProhibition(bearer, body, head)
                case "Obligation" if elem.label == "PrescriptiveStatement" => tptpObligation(bearer, body, head)
                case "Permission" if elem.label == "PrescriptiveStatement" => tptpPermission(bearer, body, head)
                case "None" if elem.label == "ConstitutiveStatement" => tptpConstitutive(body, head)
                case _ => throw  new IllegalArgumentException("typ match")
              }
            } else throw new IllegalArgumentException("if/then != 1")
          }
          TPTP.TFFAnnotated(name, role = "axiom", formula = TPTP.TFF.Logical(convertedRule), annotations)

        case "FactualStatement" =>
          val body = (elem \ "_") diff (elem \ "Paraphrase")
          val convertedFormula = if (body.size != 1) throw  new IllegalArgumentException("body != 1")
          else lrmlFormula(body.head)
          TPTP.TFFAnnotated(name, role = "axiom", formula = TPTP.TFF.Logical(convertedFormula), annotations)

        case _ => throw new IllegalArgumentException(s"Unsupported Statement type '${elem.label}' in input '${elem.toString()}'.")
      }
    }

    private def lrmlMaybeDeonticFormula(elem: xml.Node): (String, Option[TPTP.TFF.Term], TPTP.TFF.Formula) = {
      elem.label match {
        case "Obligation" | "Permission" | "Prohibition" =>
          val slot = elem \ "slot"
          val bearer = slot.find(x => (x \ "Bearer").nonEmpty) match {
            case Some(slot0) =>
              val other = (slot0 \ "_").filterNot(_.label == "Bearer")
              Some(lrmlTerm(other.head))
            case None => None
          }
          val rest = (elem \ "_").filterNot(_.label == "slot")
          if (rest.nonEmpty) (elem.label, bearer, lrmlFormula(rest.head))
          else throw new IllegalArgumentException
        case _ => ("None", None, lrmlFormula(elem))
      }
    }

    private def lrmlFormula(elem: xml.Node): TPTP.TFF.Formula = {
      fromRuleMLFormula(elem)
    }


    private def fromRuleMLFormula(formula: xml.Node): TPTP.TFF.Formula = {
      formula.label match {
        case "Data" if formula.attribute("http://www.w3.org/2001/XMLSchema-instance", "type").isDefined && formula.text.nonEmpty =>
          val ty0 = formula.attribute("http://www.w3.org/2001/XMLSchema-instance", "type").get
          if (ty0.nonEmpty && ty0.head.text.nonEmpty) {
            val ty = ty0.head.text
            ty.split(':').toSeq match {
              case Seq(left, right) =>
                val ns = formula.getNamespace(left)
                if (ns == "http://www.w3.org/2001/XMLSchema" && right == "boolean") {
                  formula.text.toLowerCase match {
                    case "true" | "1" => TPTP.TFF.AtomicFormula("$true", Seq.empty)
                    case "false" | "0" => TPTP.TFF.AtomicFormula("$false", Seq.empty)
                    case _ => throw new IllegalArgumentException(s"Unrecognized boolean data '${formula.text}'.")
                  }
                } else {
                  throw new IllegalArgumentException("Unsupported data type")
                }
              case _ => throw new IllegalArgumentException("Unsupported data type")
            }
          } else throw new IllegalArgumentException("Unsupported data type")


        case "Exists" | "Forall" =>
          val children = formula \ "_"
          val varElems = children.filter(_.label == "Var") // TODO: Add optional declare tag
          val variables = varElems.map(_.attribute("key").get.head.text)
          val body0 = children diff varElems // TODO: Add optiomal formula tag
          assert(body0.size == 1)
          val body = body0.head
          val transformedBody = fromRuleMLFormula(body)
          if (formula.label == "Exists") TPTP.TFF.QuantifiedFormula(TPTP.TFF.?, variables.map(x => (x, None)), transformedBody)
          else TPTP.TFF.QuantifiedFormula(TPTP.TFF.!, variables.map(x => (x, None)), transformedBody)

        case "And" | "Or" =>
          val children = formula \ "_"
          val transformedChildren = children.map(fromRuleMLFormula)
          if (formula.label == "And") transformedChildren.reduce(TPTP.TFF.BinaryFormula(TPTP.TFF.&, _, _))
          else transformedChildren.reduce(TPTP.TFF.BinaryFormula(TPTP.TFF.|, _, _))
        case "Implies" =>
          val lhs = (formula \ "if" \ "_").head // TODO: If-then optional
          val rhs = (formula \ "then" \ "_").head
          val transformedLeft = fromRuleMLFormula(lhs)
          val transformedRight = fromRuleMLFormula(rhs)
          TPTP.TFF.BinaryFormula(TPTP.TFF.Impl, transformedLeft, transformedRight)

        case "Equivalent" =>
          val argNodes = formula \ "_"
          assert(argNodes.length == 2)
          val arg1Pre = argNodes.head
          val arg2Pre = argNodes(1)
          val left = if (arg1Pre.label == "torso") (arg1Pre \ "_").head else arg1Pre
          val right = if (arg2Pre.label == "torso") (arg2Pre \ "_").head else arg2Pre
          val transformed1 = fromRuleMLFormula(left)
          val transformed2 = fromRuleMLFormula(right)
          TPTP.TFF.BinaryFormula(TPTP.TFF.<=>, transformed1, transformed2)

        case "Neg" =>
          val body = (formula \ "_").head // TODO: optional strong tag
          val transformedBody = fromRuleMLFormula(body)
          TPTP.TFF.UnaryFormula(TPTP.TFF.~, transformedBody)

        case "Equal" =>
          val argNodes = formula \ "_"
          assert(argNodes.length == 2)
          val arg1Pre = argNodes.head
          val arg2Pre = argNodes(1)
          val arg1 = if (arg1Pre.label == "left" || arg1Pre.label == "right") (arg1Pre \ "_").head else arg1Pre
          val arg2 = if (arg2Pre.label == "left" || arg2Pre.label == "right") (arg2Pre \ "_").head else arg2Pre
          val transformed1 = fromRuleMLAtom(arg1)
          val transformed2 = fromRuleMLAtom(arg2)
          TPTP.TFF.Equality(transformed1, transformed2)

        case "Atom" =>
          val predNode = (formula \\ "Rel").head
          val argElems = (formula \ "_").filterNot(e => e.label == "Rel" || e.label == "op")
          val predName = fromTextOrFromAttr(predNode, "iri")
          val transformedArgs = argElems.map(fromRuleMLAtom)
          TPTP.TFF.AtomicFormula(predName, transformedArgs)

        case _ => throw new IllegalArgumentException(s"Unexpected formula input: ${formula.toString()}")
      }
    }

    private def lrmlTerm(elem: xml.Node): TPTP.TFF.Term = fromRuleMLAtom(elem)

    def fromRuleMLAtom(atom: xml.Node): TPTP.TFF.Term = {
      atom.label match {
        case "Ind" | "Const" =>
          val name = if (atom.attribute("keyref").isDefined) atom.attribute("keyref").get.head.text
          else fromAttrOrFromText(atom, "key")
          TPTP.TFF.AtomicTerm(name, Seq.empty)
        case "Var" =>
          val varName = if (atom.attribute("keyref").isDefined) atom.attribute("keyref").get.head.text
          else fromAttrOrFromText(atom, "key")
          TPTP.TFF.Variable(varName)
        case "Expr" =>
          val funNode = if ((atom \ "op").isEmpty) (atom \ "Fun").head else (atom \ "op" \ "Fun").head
          val argElems = (atom \ "_").filterNot(e => e.label == "Fun" || e.label == "op")
          val funName = fromTextOrFromAttr(funNode, "iri")
          val transformedArgs = argElems.map(fromRuleMLAtom)
          TPTP.TFF.AtomicTerm(funName, transformedArgs)
        case _ => throw new RuntimeException(s"unexpected label '${atom.label}'")
      }
    }

    private def fromTextOrFromAttr(node: xml.Node, attributeName: String): String = {
      val text = node.text
      if (text.nonEmpty) text
      else {
        node.attribute(attributeName) match {
          case Some(value) if value.nonEmpty => value.head.text
          case _ => ""
        }
      }
    }
    private def fromAttrOrFromText(node: xml.Node, attributeName: String): String = {
      node.attribute(attributeName) match {
        case Some(value) if value.nonEmpty => value.head.text
        case _ => node.text
      }
    }
  }

}
